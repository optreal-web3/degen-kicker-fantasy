// Track user interaction for initial sort behavior
let hasInteracted = false;

// Sort table by column index
function sortTable(n) {
    hasInteracted = true;
    const table = document.getElementById("leaderboard");
    let switching = true;
    let dir = "desc";
    let switchcount = 0;
    
    while (switching) {
        switching = false;
        const rows = table.rows;
        for (let i = 1; i < (rows.length - 2); i += 2) {
            let shouldSwitch = false;
            const x = rows[i].getElementsByTagName("TD")[n];
            const y = rows[i + 2].getElementsByTagName("TD")[n];
            const xVal = Number(x.innerHTML);
            const yVal = Number(y.innerHTML);
            if (dir === "asc") {
                if (xVal > yVal) {
                    shouldSwitch = true;
                    break;
                }
            } else if (dir === "desc") {
                if (xVal < yVal) {
                    shouldSwitch = true;
                    break;
                }
            }
        }
        if (shouldSwitch) {
            rows[i].parentNode.insertBefore(rows[i + 2], rows[i]);
            rows[i].parentNode.insertBefore(rows[i + 3], rows[i + 1]);
            switching = true;
            switchcount++;
        } else {
            if (switchcount === 0 && dir === "desc") {
                dir = "asc";
                switching = true;
            }
        }
    }
    highlightMax();
    collapseAllDetails();
}

// Highlight maximum values in PTS, YDS, LONG, FG Made columns
function highlightMax() {
    const table = document.getElementById("leaderboard");
    const tbody = table.tBodies[0];
    const cols = [2, 3, 4, 5]; // 0-based for PTS, YDS, LONG, FG Made
    cols.forEach(col => {
        let values = [];
        Array.from(tbody.rows).forEach((row, index) => {
            if (index % 2 === 0) {
                let val = parseInt(row.cells[col].textContent.trim());
                if (!isNaN(val)) values.push(val);
            }
        });
        let max = Math.max(...values);
        Array.from(tbody.rows).forEach((row, index) => {
            if (index % 2 === 0) {
                let td = row.cells[col];
                td.classList.remove('highlight-cell');
                let val = parseInt(td.textContent.trim());
                if (val === max) {
                    td.classList.add('highlight-cell');
                }
            }
        });
    });
}

// Collapse all kicker details
function collapseAllDetails() {
    document.querySelectorAll('.kicker-details').forEach(details => {
        details.style.display = 'none';
    });
}

// Initial sort to show top performers
function initialSort() {
    const table = document.getElementById("leaderboard");
    const tbody = table.tBodies[0];
    const rows = Array.from(tbody.rows).filter((row, index) => index % 2 === 0);
    const detailsRows = Array.from(tbody.rows).filter((row, index) => index % 2 !== 0);

    // Find top rows for PTS, YDS, LONG, FG Made
    let sortedRows = [
        rows.find(row => parseInt(row.cells[2].textContent) === Math.max(...rows.map(r => parseInt(r.cells[2].textContent)))),
        rows.find(row => parseInt(row.cells[3].textContent) === Math.max(...rows.map(r => parseInt(r.cells[3].textContent)))),
        rows.find(row => parseInt(row.cells[4].textContent) === Math.max(...rows.map(r => parseInt(r.cells[4].textContent)))),
        rows.find(row => parseInt(row.cells[5].textContent) === Math.max(...rows.map(r => parseInt(r.cells[5].textContent))))
    ];

    // Remove duplicates while preserving order
    sortedRows = sortedRows.filter((row, index, self) => self.indexOf(row) === index);

    // Sort remaining rows by PTS
    let remainingRows = rows.filter(row => !sortedRows.includes(row));
    remainingRows.sort((a, b) => parseInt(b.cells[2].textContent) - parseInt(a.cells[2].textContent));

    // Combine rows with their details
    let finalRows = [];
    sortedRows.concat(remainingRows).forEach(row => {
        finalRows.push(row);
        let wallet = row.dataset.wallet;
        let detailRow = detailsRows.find(dr => dr.dataset.wallet === wallet);
        if (detailRow) finalRows.push(detailRow);
    });

    // Clear and repopulate tbody
    while (tbody.firstChild) tbody.removeChild(tbody.firstChild);
    finalRows.forEach(row => tbody.appendChild(row));

    highlightMax();

    // Show details for top PTS leader on initial load
    if (!hasInteracted) {
        const topDetails = tbody.rows[1];
        if (topDetails && topDetails.classList.contains('kicker-details')) {
            topDetails.style.display = 'table-row';
        }
    }
}

// Fetch and update data for selected week
async function fetchWeekData(week) {
    // Check localStorage first
    const cached = localStorage.getItem(`week_${week}`);
    if (cached) return JSON.parse(cached);

    // Fetch from Vercel
    try {
        const response = await fetch(`/data/week_${week}.json`);
        if (!response.ok) throw new Error('Failed to fetch data');
        const data = await response.json();
        localStorage.setItem(`week_${week}`, JSON.stringify(data));
        return data;
    } catch (error) {
        console.error('Error fetching week data:', error);
        return null;
    }
}

// Update kicker showcase and leaderboard
function updateUI(data, week) {
    if (!data || data.length === 0) {
        document.querySelectorAll('.kicker-card').forEach(card => {
            card.querySelector('h3').textContent = 'No Data';
            card.querySelectorAll('p').forEach(p => p.textContent = 'N/A');
        });
        const tbody = document.querySelector('#leaderboard tbody');
        tbody.innerHTML = '<tr><td colspan="6">No data available</td></tr>';
        return;
    }

    // Update kicker showcase
    const kickers = data;
    const topPts = kickers.sort((a, b) => b.pts - a.pts)[0] || {};
    const topYds = kickers.sort((a, b) => b.yds - a.yds)[0] || {};
    const topLong = kickers.sort((a, b) => b.long - a.long)[0] || {};

    const updateCard = (selector, kicker) => {
        const card = document.querySelector(`.kicker-card[data-kicker="${selector}"]`);
        if (card && kicker.kicker) {
            card.querySelector('h3').textContent = kicker.kicker;
            const [p1, p2] = card.querySelectorAll('p');
            p1.textContent = `Fantasy PTS: ${kicker.pts || 'N/A'}`;
            p2.textContent = `FG Made: ${kicker.fg_made || 'N/A'}`;
            if (kicker.team) {
                card.querySelector('.team-logo').src = `https://a.espncdn.com/i/teamlogos/nfl/500/${kicker.team.toLowerCase()}.png`;
            }
        }
    };

    updateCard('top-pts', topPts);
    updateCard('top-yds', topYds);
    updateCard('top-long', topLong);

    // Update leaderboard (mock wallet data; replace with actual wallet mappings)
    const tbody = document.querySelector('#leaderboard tbody');
    tbody.innerHTML = '';
    const mockWallets = [
        { wallet: '0x1a2b3...a6b7c', teams: ['buf', 'cin', 'cle'], kickers: ['Kicker X', 'Kicker Y', 'Kicker Z'] },
        { wallet: '0x2b3c4...b7c8d', teams: ['den', 'ind', 'jax'], kickers: ['Kicker P', 'Kicker Q', 'Kicker R'] },
    ];
    mockWallets.forEach((entry, index) => {
        const kicker = kickers[index % kickers.length] || {};
        const row = document.createElement('tr');
        row.classList.add('wallet-row');
        row.dataset.wallet = entry.wallet;
        row.innerHTML = `
            <td>${entry.wallet}</td>
            <td>${entry.teams.map(team => `<img src="https://a.espncdn.com/i/teamlogos/nfl/500/${team}.png" alt="${team} Logo" class="team-logo">`).join('')}</td>
            <td>${kicker.pts || 0}</td>
            <td>${kicker.yds || 0}</td>
            <td>${kicker.long || 0}</td>
            <td>${kicker.fg_made || 0}</td>
        `;
        const detailRow = document.createElement('tr');
        detailRow.classList.add('kicker-details');
        detailRow.dataset.wallet = entry.wallet;
        detailRow.innerHTML = `<td colspan="6">${entry.kickers.join(', ')}</td>`;
        tbody.appendChild(row);
        tbody.appendChild(detailRow);
    });

    initialSort();
}

// Initialize page
document.addEventListener('DOMContentLoaded', async function() {
    initialSort();
    document.querySelectorAll('#leaderboard tbody tr.wallet-row').forEach(row => {
        row.addEventListener('click', () => {
            hasInteracted = true;
            const wallet = row.dataset.wallet;
            const details = row.nextElementSibling;
            if (details && details.classList.contains('kicker-details') && details.dataset.wallet === wallet) {
                details.style.display = details.style.display === 'none' || details.style.display === '' ? 'table-row' : 'none';
            }
            document.querySelectorAll('.kicker-details').forEach(otherDetails => {
                if (otherDetails !== details) {
                    otherDetails.style.display = 'none';
                }
            });
        });
    });
    document.querySelectorAll('#leaderboard th.sortable').forEach((th, index) => {
        th.addEventListener('click', () => sortTable(index + 2));
    });

    // Week selector
    const weekSelect = document.getElementById('week-select');
    const loadWeek = async () => {
        const week = weekSelect.value;
        const data = await fetchWeekData(week);
        updateUI(data, week);
    };
    weekSelect.addEventListener('change', loadWeek);
    await loadWeek(); // Load initial week
});
```
