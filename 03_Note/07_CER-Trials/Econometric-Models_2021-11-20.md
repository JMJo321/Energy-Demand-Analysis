## Econometric Models for CER Trials

### 1. Old Models

1. Models for the Average Treatment Effect
   $$
   \begin{align*}
   kWh_{itpw} \ 
   & = \ \beta_{1} \cdot 1[\text{Treatment}]_{i} \ + \ \beta_{2} \cdot 1[\text{Post}]_{t} \ + \ \beta_{3} \cdot 1[\text{Treatment \& Post}]_{it} \\
   & \hspace{0.7cm} + \ \beta_{4} \cdot HDD_{t} \cdot 1[\text{Treatment}]_{i} \ + \ \beta_{5} \cdot HDD_{t} \cdot 1[\text{Post}]_{t} \\
   & \hspace{0.7cm} + \ \beta_{6} \cdot HDD_{t} \cdot 1[\text{Treatment \& Post}]_{it} \\
   & \hspace{0.7cm} + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \delta_{mw} \ + \ \epsilon_{itpw}
   \end{align*}
   $$
   

2. Models for By-Rate-Change Treatment Effect

$$
\begin{align*}
kWh_{itpw} \ 
& = \ \beta_{1} \cdot 1[\text{Treatment}]_{i} \cdot \Delta R_{ip} \\
& \hspace{0.7cm} + \ \beta_{2} \cdot 1[\text{Post}]_{t} \\
& \hspace{0.7cm} + \ \beta_{3} \cdot 1[\text{Treatment \& Post}]_{it} \cdot \Delta R_{ip} \\
& \hspace{0.7cm} + \ \beta_{4} \cdot HDD_{t} \cdot 1[\text{Treatment}]_{i} \cdot \Delta R_{ip} \\
& \hspace{0.7cm} + \ \beta_{5} \cdot HDD_{t} \cdot 1[\text{Post}]_{t} \\
& \hspace{0.7cm} + \ \beta_{6} \cdot HDD_{t} \cdot 1[\text{Treatment \& Post}]_{it} \cdot \Delta R_{ip} \\
& \hspace{0.7cm} + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \delta_{mw} \ + \ \epsilon_{itpw}
\end{align*}
$$



### 2. New Models

1. Models for the Average Treatment Effect
   $$
   \begin{align*}
   kWh_{itpw} \ 
   & = \ \beta_{1} \cdot 1[\text{Treatment}]_{i} \ + \ \beta_{2} \cdot 1[\text{Post}]_{t} \ + \ \beta_{3} \cdot 1[\text{Treatment \& Post}]_{it} \\
   & \hspace{0.7cm} + \ \beta_{4} \cdot HDD_{t} \\
   & \hspace{0.7cm} + \ \beta_{5} \cdot HDD_{t} \cdot 1[\text{Treatment}]_{i} \ + \ \beta_{6} \cdot HDD_{t} \cdot 1[\text{Post}]_{t} \\
   & \hspace{0.7cm} + \ \beta_{7} \cdot HDD_{t} \cdot 1[\text{Treatment \& Post}]_{it} \\
   & \hspace{0.7cm} + \ \alpha_{dw} \ + \ \gamma_{mw} \ + \ \epsilon_{itpw}
   \end{align*}
   $$
   

2. Models for By-Rate-Change Treatment Effect
   $$
   \begin{align*}
   kWh_{itpw} \ 
   & = \ \beta_{1} \cdot 1[\text{Treatment}]_{i} \ + \ \beta_{2} \cdot 1[\text{Treatment}]_{i} \cdot \Delta R_{ip} \\
   & \hspace{0.7cm} + \ \beta_{3} \cdot 1[\text{Post}]_{t} \\
   & \hspace{0.7cm} + \ \beta_{4} \cdot 1[\text{Treatment \& Post}]_{it} \ +\ \beta_{5} \cdot 1[\text{Treatment \& Post}]_{it} \cdot \Delta R_{ip} \\
   & \hspace{0.7cm} + \ \beta_{6} \cdot HDD_{t} \\
   & \hspace{0.7cm} + \ \beta_{7} \cdot HDD_{t} \cdot 1[\text{Treatment}]_{i} \ + \beta_{8} \cdot HDD_{t} \cdot 1[\text{Treatment}]_{i} \cdot \Delta R_{ip} \\
   & \hspace{0.7cm} + \ \beta_{9} \cdot HDD_{t} \cdot 1[\text{Post}]_{t} \\
   & \hspace{0.7cm} + \ \beta_{10} \cdot HDD_{t} \cdot 1[\text{Treatment \& Post}]_{it} \ + \ \beta_{11} \cdot HDD_{t} \cdot 1[\text{Treatment \& Post}]_{it} \cdot \Delta R_{ip} \\
   & \hspace{0.7cm} + \ \alpha_{dw} \ + \ \gamma_{mw} \ + \ \epsilon_{itpw}
   \end{align*}
   $$



In the models above, the meaning of each subscription is as follows:

- $i$ :  Household
- $t$ :  Day
- $w$ :  30-Mintue Interval, $w \in \{ 1, 2, \cdots, 48 \}$
- $d$ :  Day of Week
- $m$ :  Month of Year
- $p$ :  Rate Period, $p \in \{ \text{Night, Day: Pre-Peak, Peak, Day: Post-Peak} \}$
- $\tau$ :  Tariff Allocation, $\tau \in \{A, B, C, D, E\}$ where $E$ means the Rate of the Control Group (i.e., Flat Rate)
- $\Delta R_{ip}$ :  Difference from the Rate of the Control Group, i.e. $(\text{Rate})_{p \tau} \ - \ (\text{Flat Rate})$

