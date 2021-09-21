

$$
\begin{align*}
kWh_{itpw} \ 
& = \ \beta_{1} \cdot 1[\text{Treatment}]_{it} \cdot \Delta R \ + \ \beta_{2} \cdot 1[\text{Post}]_{it} \ + \ \beta_{3} \cdot 1[\text{Treatment \& Post}]_{it} \cdot \Delta R \\
& \hspace{0.6cm} + \ \beta_{4} \cdot HDD_{t} \cdot 1[\text{Treatment}]_{it} \cdot \Delta R \ + \ \beta_{5} \cdot HDD_{t} \cdot 1[\text{Post}]_{it} \ + \ \beta_{6} \cdot HDD_{t} \cdot 1[\text{Treatment \& Post}]_{it} \cdot \Delta R \\
& \hspace{1.1cm} + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \delta_{mw} \ + \ \epsilon_{itpw}
\end{align*}
$$
where

- $i$ :  Household
- $t$ :  Day
- $w$ :  30-Mintue Interval, $w \in \{ 1, 2, \cdots, 48 \}$
- $d$ :  Day of Week
- $m$ :  Month of Year
- $p$ :  Rate Period, $p \in \{ \text{Night, Day: Pre-Peak, Peak, Day: Post-Peak} \}$
- $\tau$ :  Tariff Allocation, $\tau \in \{A, B, C, D, E\}$ where $E$ means the Rate of the Control Group (i.e., Flat Rate)
- $\Delta R$ :  Difference from the Rate of the Control Group, i.e. $(\text{Rate})_{p \tau} \ - \ (\text{Flat Rate})$

