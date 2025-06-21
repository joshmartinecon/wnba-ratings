# WNBA Ratings & Optimal Rotations

This repository powers a [Shiny app](https://joshmartinecon.shinyapps.io/wnba-explorer/) that ranks WNBA players using advanced statistics and estimates optimal playing time allocations across teams. The app helps visualize how team and player performance might change under re-optimized rotations based on actual productivity.

## Overview

The app features:
  
  - Individual player ratings on a 60–100 scale
  - Estimated optimal minutes per game (MPG*)
  - Gaps between current and optimal usage (Δ Minutes)
  - Team-level ratings based on both observed and optimized playing time

All data are scraped from [Basketball Reference](https://www.basketball-reference.com) and [ESPN](https://www.espn.com/wnba/injuries), then processed using R. The results are published in this GitHub repository and dynamically loaded into the Shiny app.

## Dynamic Updating

The Shiny app draws directly from the `.csv` files hosted in this repository. When the data are updated and pushed to GitHub, the app automatically reflects those changes upon reload.

---

## Methodology

### Data Collection & Roster Alignment

Data are collected from Basketball Reference’s advanced stats page for the current season. Each player’s stats, team, and unique ID are matched to team rosters to ensure that only players currently on active rosters are retained.

To filter out players with insufficient sample sizes, only those with at least 40 minutes played are included.

### Minutes-Per-Game Adjustment

To standardize team playing time (given that players miss some games), each team’s total minutes are adjusted so that the sum of per-player minutes equals exactly 200 (5 players × 40 minutes):

$$
\textstyle \text{Adj}_k=\frac{\sum_{i\in k}\frac{MP_i}{G_i}}{40\times5}
\quad\Rightarrow\quad
\textstyle MPG_i=\frac{MP_i/G_i}{\text{Adj}_k}
$$

### Productivity Score

Three advanced metrics are used:
  
  - Player Efficiency Rating (PER)
  - Net Rating (ORtg − DRtg)
  - Win Shares (WS)

These are scaled by minutes per game and standardized across the league:

$$
\tilde{x}_{ij}=x_{ij}\times MPG_i,
\quad
z_{ij}=\frac{\tilde{x}_{ij}-\bar{\tilde{x}}_j}{\sigma_{\tilde{x}_j}},
\quad
s_i = \frac{1}{3} \sum_{j=1}^3 z_{ij}
$$

The result is a composite productivity score per player.

### Minutes–Productivity Curve

I estimate the minutes each player *should* play to reflect their observed productivity. A quadratic regression is fitted:

$$
s_i = \alpha + \beta_1 \cdot MPG_i + \beta_2 \cdot MPG_i^2
$$

Solving for minutes \( MPG^*_i \) consistent with observed performance:

$$
MPG^*_i = \frac{-\beta_1 + \sqrt{\beta_1^2 - 4\beta_2(\alpha - s_i)}}{2\beta_2}
$$

Estimated values are scaled so that team minutes sum to 200, capped at the league maximum, and iteratively redistributed when constraints are violated.

### Injuries & Availability

Injury status is pulled from ESPN’s WNBA injury tracker. Players listed as “Out” are excluded from optimization, and then re-added to the dataset with:
  
  - `MPG = 0`
  - `MPG* = 0`
  - Player names marked with a `+`

### Rating Scale

Player productivity scores \( s_i \) are transformed onto a 60–100 scale using an inverse hyperbolic sine transformation to emphasize differentiation while compressing extreme outliers:

$$
\textstyle Rating_i = 60 + \frac{\operatorname{asinh}(s_i) - \min \operatorname{asinh}(s)}{\max \operatorname{asinh}(s) - \min \operatorname{asinh}(s)} \times 40
$$

### Team Ratings

Two metrics summarize team strength:

$$
\begin{aligned}
\text{Rating}_k &= \frac{\sum_i Rating_i \cdot MPG_i}{\sum MPG_i} \\
\text{Rating}_k^* &= \frac{\sum_i Rating_i \cdot MPG^*_i}{\sum MPG^*_i}
\end{aligned}
$$

Where:
  
  - \( \text{Rating}_k \) is based on actual usage
  - \( \text{Rating}_k^* \) reflects optimized rotations

Teams are ranked by \( \text{Rating}_k^* \) in the Shiny app.
