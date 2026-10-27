# NHL Shift Length and Expected Goal Performance

An independent sports analytics project examining the relationship between NHL shift length and team expected-goal performance during the 2025–26 NHL season.

## Project Overview

NHL broadcasts frequently highlight players who have been stuck on the ice for an extended shift, but how much of a disadvantage does this actually represent?

This project investigates how team expected-goal performance changes as players remain on the ice longer, and whether the relationship differs between forwards and defensemen.

Using R, I combined NHL player shift data, play-by-play events, and MoneyPuck expected-goal data to reconstruct shifts and analyze team performance across five-second intervals.

## Key Findings

- **Long shifts are associated with worse team performance.** During 5v5 play, teams with an average unit shift age of 60 seconds or more generated 38.2% less xG and allowed 25.9% more xG than during the first 30 seconds of a shift.
- **The disadvantage is substantial.** The percentage increase in xGA associated with extended shifts was approximately 40% as large as the increase associated with playing on the penalty kill.
- **Defensemen shift age is a stronger negative indicator.** After accounting for forward shift age, longer defensemen shifts were associated with a substantially greater decline in team xG margin than longer forward shifts.

These findings describe statistical associations rather than causal effects, as extended shifts may reflect player fatigue, unfavorable game situations, or both.

## Tools and Methods

**Languages and tools:** R, data.table, ggplot2, Quarto, gt

**Statistical methods:** Linear regression, game-clustered standard errors, bootstrap confidence intervals, and positional comparisons.

**Data sources:** NHL API, hockeyR, and MoneyPuck.

## Repository Structure

- `index.qmd` — Complete research manuscript
- `R/` — Data collection, processing, and validation scripts
- `notebooks/` — Statistical analysis and visualization code
- `images/` — Figures and summary tables
- `references.bib` — Research references and data-source citations

## Read the Full Paper

The complete manuscript is available in `index.qmd`.

A formatted HTML version will be available through GitHub Pages.

## Acknowledgements

This manuscript was prepared using the [JSDSS manuscript template](https://github.com/jsds-sports/jsdss-manuscript-template).