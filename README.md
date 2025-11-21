# COPC Experience Tracker

A Shiny app to track personal daily experiences across different times of day.

**COPC**: Create, Observe, Participate, Consume

## Features

- **Log Experiences**: Capture one or many experience types with descriptions
- **Time of Day**: Track experiences across morning, day, evening, and night
- **Daily Snapshot**: View all experiences for any selected date
- **Trend Analysis**: Visualize patterns over time
  - Weekly (last 7 days)
  - Monthly (last 30 days)
  - Year to Date
  - Full Year (last 12 months)
- **History View**: Filter and browse all logged experiences

## Experience Types

- Work/Productivity
- Exercise/Physical
- Social/Connection
- Creative/Hobby
- Learning/Growth
- Rest/Relaxation
- Nature/Outdoors
- Mindfulness/Meditation
- Entertainment
- Family Time
- Self-Care
- Other

## Data Storage

Data is stored in CSV format (`data/experiences.csv`) for GitHub compatibility:

- **Human-readable**: Easy to review changes in diffs
- **Version controlled**: Track your experience history over time
- **Portable**: Simple to backup, export, or migrate
- **Editable**: Can be modified manually if needed

### Privacy Note

By default, the experience data is tracked in git. If you want to keep your data private while still using GitHub:

1. Add `data/experiences.csv` to `.gitignore`
2. Or use a private repository

## Installation

### Prerequisites

- R (>= 4.0)
- Required packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "tidyverse",
  "lubridate",
  "plotly"
))
```

### Running the App

```r
# From R console
shiny::runApp()

# Or from command line
Rscript -e "shiny::runApp()"
```

## Usage

1. **Log an Experience**
   - Select the date (defaults to today)
   - Choose the time of day
   - Select one or more experience types
   - Add an optional description
   - Click "Save Experience"

2. **View Daily Snapshot**
   - Navigate to the "Daily Snapshot" tab
   - Select a date to see all experiences for that day
   - Delete individual entries if needed

3. **Analyze Trends**
   - Go to the "Trends" tab
   - Select time period (week, month, YTD, year)
   - Choose visualization type:
     - Experience Types: Bar chart of type distribution
     - Time of Day: Pie chart of when you log experiences
     - Daily Activity: Line chart of experiences over time

4. **Browse History**
   - Use the "History" tab
   - Filter by experience type, time of day, or date range
   - View up to 100 most recent matching entries

## Project Structure

```
copc/
├── app.R              # Main Shiny application
├── data/
│   └── experiences.csv  # Experience log (CSV format)
├── .gitignore
└── README.md
```

## Contributing

Feel free to customize the experience types in `app.R` by modifying the `EXPERIENCE_TYPES` vector to match your personal tracking needs.

## License

MIT
