import requests
import json
import os
from datetime import datetime

# Define season and weeks (up to week 5 as of October 07, 2025)
YEAR = 2025
WEEKS = range(1, 6)

def get_game_ids(week):
    url = f"https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?week={week}&seasontype=2&season={YEAR}"
    try:
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        data = response.json()
        return [event['id'] for event in data.get('events', [])]
    except Exception as e:
        print(f"Error fetching game IDs for week {week}: {e}")
        return []

def get_pbp(game_id):
    url = f"https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={game_id}"
    try:
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        return response.json().get('plays', [])
    except Exception as e:
        print(f"Error fetching PBP for game {game_id}: {e}")
        return []

def calculate_kicker_stats(plays):
    kicker_stats = {}
    for play in plays:
        text = play.get('text', '').lower()
        if 'field goal' in text and 'good' in text:
            parts = text.split()
            kicker_name = ' '.join(parts[:2])  # Approximate, refine if needed
            dist = int(parts[parts.index('yd') - 1]) if 'yd' in parts else 0
            pts = 3 if dist < 40 else 4 if dist < 50 else 5
            if kicker_name not in kicker_stats:
                kicker_stats[kicker_name] = {'fg_made': 0, 'pts': 0, 'yds': 0, 'long': 0, 'team': ''}
            kicker_stats[kicker_name]['fg_made'] += 1
            kicker_stats[kicker_name]['pts'] += pts
            kicker_stats[kicker_name]['yds'] += dist
            kicker_stats[kicker_name]['long'] = max(kicker_stats[kicker_name]['long'], dist)
            kicker_stats[kicker_name]['team'] = play.get('possession', {}).get('team', {}).get('abbreviation', '')
        elif 'extra point' in text and 'good' in text:
            parts = text.split()
            kicker_name = ' '.join(parts[:2])
            if kicker_name not in kicker_stats:
                kicker_stats[kicker_name] = {'fg_made': 0, 'pts': 0, 'yds': 0, 'long': 0, 'team': ''}
            kicker_stats[kicker_name]['pts'] += 1
            kicker_stats[kicker_name]['team'] = play.get('possession', {}).get('team', {}).get('abbreviation', '')
    return [{'kicker': k, **v} for k, v in kicker_stats.items()]

# Ensure data directory exists
os.makedirs('data', exist_ok=True)

for week in WEEKS:
    stats = []
    game_ids = get_game_ids(week)
    for game_id in game_ids:
        plays = get_pbp(game_id)
        week_stats = calculate_kicker_stats(plays)
        stats.extend(week_stats)
    
    # Remove duplicates by kicker name
    unique_stats = {}
    for entry in stats:
        if entry['kicker'] not in unique_stats or unique_stats[entry['kicker']]['pts'] < entry['pts']:
            unique_stats[entry['kicker']] = entry
    stats = list(unique_stats.values())
    
    # Save to JSON
    with open(f'data/week_{week}.json', 'w') as f:
        json.dump(stats, f, indent=2)
    
    print(f"Week {week} processed: {len(stats)} kickers")

# Initial commit to trigger Vercel
repo_dir = os.path.dirname(os.path.abspath(__file__))
if os.path.exists(os.path.join(repo_dir, '.git')):
    os.system('git config user.name "GitHub Action"')
    os.system('git config user.email "action@github.com"')
    os.system('git add data/*')
    os.system(f'git commit -m "Update kicker data for week {max(WEEKS)}" || echo "No changes to commit"')
    os.system('git push origin main')
