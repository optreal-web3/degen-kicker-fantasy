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

// Highlight maximum values in PTS, YDS, LONG columns
function highlightMax() {
    const table = document.getElementById("leaderboard");
    const tbody = table.tBodies[0];
    const cols = [2, 3, 4]; // 0-based for PTS, YDS, LONG
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

    // Find top rows for PTS, YDS, LONG
    let sortedRows = [
        rows.find(row => parseInt(row.cells[2].textContent) === Math.max(...rows.map(r => parseInt(r.cells[2].textContent)))),
        rows.find(row => parseInt(row.cells[3].textContent) === Math.max(...rows.map(r => parseInt(r.cells[3].textContent)))),
        rows.find(row => parseInt(row.cells[4].textContent) === Math.max(...rows.map(r => parseInt(r.cells[4].textContent))))
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

// Placeholder for future API data fetching
function fetchKickerData() {
    // TODO: Add API call (e.g., to nflfastR or Render backend)
    // Example: fetch('https://your-api-endpoint').then(response => response.json()).then(data => { /* Update table */ });
    console.log("Placeholder for fetching dynamic kicker data");
}

// Initialize page
document.addEventListener('DOMContentLoaded', function() {
    initialSort();
    document.querySelectorAll('#leaderboard tbody tr.wallet-row').forEach(row => {
        row.addEventListener('click', () => {
            hasInteracted = true;
            const wallet = row.dataset.wallet;
            const details = row.nextElementSibling;
            if (details && details.classList.contains('kicker-details') && details.dataset.wallet === wallet) {
                details.style.display = details.style.display === 'none' || details.style.display === '' ? 'table-row' : 'none';
            }
            // Collapse all other details
            document.querySelectorAll('.kicker-details').forEach(otherDetails => {
                if (otherDetails !== details) {
                    otherDetails.style.display = 'none';
                }
            });
        });
    });
    // Add click event listeners for sortable table headers
    document.querySelectorAll('#leaderboard th.sortable').forEach((th, index) => {
        th.addEventListener('click', () => sortTable(index + 2));
    });
});
