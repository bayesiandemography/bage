# 2. Mathematical Details

## 1 Introduction

Specification document - a mathematical description of models used by
bage.

Note: some features described here have not been implemented yet.

## 2 Input data

- outcome variable: events, numbers of people, or some sort of measure
  on a continuous variable such as income or expenditure
- exposure/size/weights
- disagg by one or more variables. Almost always includes age,
  sex/gender, and time. May include other variables eg region,
  ethnicity, education.
- not all combinations of variables present; may be some missing values

## 3 Models

### 3.1 Poisson likelihood

Let \\y_i\\ be a count of events in cell \\i = 1, \cdots, n\\ and let
\\w_i\\ be the corresponding exposure measure, with the possibility that
\\w_i \equiv 1\\. The likelihood under the Poisson model is then
\\\begin{align} y_i & \sim \text{Poisson}(\gamma_i w_i) \tag{3.1} \\
\gamma_i & \sim \text{Gamma}\left(\xi^{-1}, (\mu_i \xi)^{-1}\right),
\tag{3.2} \end{align}\\ using the shape-rates parameterisation of the
Gamma distribution. Parameter \\\xi\\ governs dispersion, with
\\\begin{equation} \text{var}(\gamma_i \mid \mu_i, \xi) = \xi \mu_i^2
\end{equation}\\ and \\\begin{equation} \text{var}(y_i \mid \mu_i, \xi,
w_i) = (1 + \xi \mu_i w_i ) \times \mu_i w_i. \end{equation}\\ We allow
\\\xi\\ to equal 0, in which case the model reduces to
\\\begin{equation} y_i \sim \text{Poisson}(\mu_i w_i). \end{equation}\\

For \\\xi \> 0\\, Equations [(3.1)](#eq:lik-pois-1) and
[(3.2)](#eq:lik-pois-2) are equivalent to \\\begin{equation} y_i \sim
\text{NegBinom}\left(\xi^{-1}, \[1 + \mu_i w_i \xi\]^{-1}\right)
\end{equation}\\ (Norton, Christen, and Fox 2018; Simpson 2022). This is
the format we use internally for estimation. When values for
\\\gamma_i\\ are needed, we generate them on the fly, using the fact
that \\\begin{equation} \gamma_i \mid y_i, w_i, \mu_i, \xi \sim
\text{Gamma}\left(y_i + \xi^{-1}, w_i + (\xi \mu_i)^{-1}\right).
\end{equation}\\

### 3.2 Binomial likelihood

The likelihood under the binomial model is \\\begin{align} y_i & \sim
\text{Binomial}(w_i, \gamma_i) \tag{3.3} \\ \gamma_i & \sim
\text{Beta}\left(\xi^{-1} \mu_i, \xi^{-1}(1 - \mu_i)\right). \tag{3.4}
\end{align}\\ Parameter \\\xi\\ again governs dispersion, with
\\\begin{equation} \text{var}(\gamma_i \mid \mu_i, \xi) = \frac{\xi}{1 +
\xi} \times \mu_i (1 -\mu_i) \end{equation}\\ and \\\begin{equation}
\text{var}(y_i \mid w_i, \mu_i, \xi) = \frac{\xi w_i + 1}{\xi + 1}
\times w_i \mu_i (1 - \mu_i). \end{equation}\\

We allow \\\xi\\ to equal 0, in which case the model reduces to
\\\begin{equation} y_i \sim \text{Binom}(w_i, \mu_i). \end{equation}\\
Equations [(3.3)](#eq:lik-binom-1) and [(3.4)](#eq:lik-binom-2) are
equivalent to \\\begin{equation} y_i \sim \text{BetaBinom}\left(w_i,
\xi^{-1} \mu_i, \xi^{-1} (1 - \mu_i) \right), \end{equation}\\ which is
what we use internally. Values for \\\gamma_i\\ can be generated using
\\\begin{equation} \gamma_i \mid y_i, w_i, \mu_i, \xi \sim
\text{Beta}\left(y_i + \xi^{-1} \mu_i, w_i - y_i + \xi^{-1}(1-\mu_i)
\right). \end{equation}\\

### 3.3 Normal likelihood

The normal model is \\\begin{align} y_i & \sim \text{N}(\gamma_i,
w_i^{-1} \sigma^2) \\ \gamma_i & = \bar{y} + s \mu_i \\ \sigma^2 & =
\bar{w} s^2 \xi^2 \end{align}\\ where \\\begin{align} \bar{y} & =
\frac{\sum\_{i=1}^n y_i}{n} \\ s & = \sqrt{\frac{\sum\_{i=1}^n (y_i -
\bar{y})^2}{n-1}} \\ \bar{w} & = \frac{\sum\_{i=1}^n w_i}{n}.
\end{align}\\

Mean \\\mu_i\\ and stanard deviation \\\xi\\ are defined on scale that
used standardized versinos of \\y\\ and \\w\\. Standardizing allows us
to apply the same priors as we use for the Poisson and binomial models.

## 4 Model for prior means

Let \\\pmb{\mu} = (\mu_1, \cdots, \mu_n)^{\top}\\. Our model for
\\\pmb{\mu}\\ is \\\begin{equation} \pmb{\mu} = \sum\_{m=0}^{M}
\pmb{X}^{(m)} \pmb{\beta}^{(m)} + \pmb{Z} \pmb{\zeta} \tag{4.1}
\end{equation}\\ where

- \\\beta^{(0)}\\ is an intercept;
- \\\pmb{\beta}^{(m)}\\, \\m=1,\cdots,M\\ is a vector with \\J_m\\
  elements describing a main effect or interaction formed from the
  dimensions of data \\\pmb{y}\\;
- \\\pmb{X}^{(m)}\\ is an \\n \times J_m\\ matrix of 1s and 0s, the
  \\i\\th row of which picks out the element of \\\pmb{\beta}^{(m)}\\
  that is used with cell \\i\\;
- \\\pmb{Z}\\ is a \\n \times P\\ matrix of covariates; and
- \\\pmb{\zeta}\\ is a coefficient vector with \\P\\ elements.

## 5 Priors for Intercept, Main Effects, and Interactions

### 5.1 General features

#### 5.1.1 ‘Along’ and ‘by’ dimensions

Each \\\pmb{\beta}^{(m)}\\, \\m \> 0\\, can be a main effect, involving
a single dimension, or an interaction, involving two dimensions. Some
priors, when applied to an interaction, treat one dimension, referred to
as the ‘along’ dimension, differently from the remaining dimensions,
referred to as ‘by’ dimensions. A random walk prior (Section
[5.4](#sec:pr-rw)), for instance, consists of an independent random walk
along the ‘along’ dimension, within each combination of the ‘by’
dimensions.

We use \\v = 1, \cdots, V_m\\ to denote position within the ‘along’
dimension, and \\u = 1, \cdots, V_m\\ to denote position within a
classification formed by the ‘by’ dimensions. When there are no
sum-to-zero constraints (see below), \\U_m = \prod_k d_k\\ where \\d_k\\
is the number of elements in the \\k\\th ‘by’ variable. When there are
sum-to-zero constraints, \\U_m = \prod_k (d_k - 1)\\.

If a prior involves an ‘along’ dimension but the user does not specify
one, the procedure for choosing a dimension is as follows:

- if the term involves time, use the time dimension;
- otherwise, if the term involves age, use the age dimension;
- otherwise, raise an error asking the user to explicitly specify a
  dimension.

#### 5.1.2 Constraints

With some combinations of terms and priors, some \\\pmb{\beta}^{(m)}\\
are only weakly identified, and have diffuse prior distributions. Even
when this happens, however, the quantity \\\mu_i = \sum\_{m=0}^M
\beta\_{j_i^m}^{(m)}\\ is still well identified, so the weak
identification may not matter to the aims of the analysis.

If, however, stronger identification is required, it can be achieved by
imposing constraints on the elements of the \\\pmb{\beta}^{(m)}\\. This
is done via the `con` argument. At present only two choices for `con`
have been implemented. The first is `"none"`, where no constraints are
applied. This is the default. The second is `"by"`.

The `"by"` option can only be used if \\\pmb{\beta}^{(m)}\\ has an
‘along’ dimension. If `con` is `"by"`, then within each element \\v\\ of
the ‘along’ dimension, the sum of the \\\beta_j^{(m)}\\ across each ‘by’
dimension is zero. For instance, if \\\pmb{\beta}^{(m)}\\ is an
interaction between time, region, and sex, with time as the ‘along’
variable, then within each combination of time and region, the values
for females and males sum to zero, and within each combination of time
and sex, the values for regions sum to zero.

Except in the case of dynamic SVD-based priors (eg Sections
[5.16](#sec:pr-svd-rw)), `"by"` constraints are implemented internally
by drawing values within an unrestricted lower-dimensional space, and
then transforming to the restricted higher-dimensional space. For
instance, a random walk prior for a time-region interaction with \\R\\
regions consists of \\R-1\\ unrestricted random walks along time, which
are converted into \\R\\ random walks that sum to zero across region.
Matrices for transforming between the unrestricted and restricted spaces
are constructed using the QR decomposition, as described in Section
1.8.1 of Wood (2017). With dynamic SVD-based priors, we draw values for
the SVD coefficients with no constraints, convert these to unconstrained
values for \\\pmb{\beta}^{(m)}\\, and then subtract means.

#### 5.1.3 Algorithm for assigning default priors

- If \\\pmb{\beta}^{(m)}\\ has one or two elements, assign
  \\\pmb{\beta}^{(m)}\\ a fixed-normal prior (Section
  [5.3](#sec:pr-fnorm));
- otherwise, if \\\pmb{\beta}^{(m)}\\ involves time, assign
  \\\pmb{\beta}^{(m)}\\ a random walk prior (Section [5.4](#sec:pr-rw))
  along the time dimension;
- otherwise, if \\\pmb{\beta}^{(m)}\\ involves age, assign
  \\\pmb{\beta}^{(m)}\\ a random walk prior (Section [5.4](#sec:pr-rw))
  along the age dimension;
- otherwise, assign \\\pmb{\beta}^{(m)}\\ a normal prior (Section
  [5.2](#sec:pr-norm))

The intercept term \\\pmb{\beta}^{(0)}\\ can only be given a
fixed-normal prior (Section [5.3](#sec:pr-fnorm)) or a Known prior
(Section [5.18](#sec:pr-known)).

### 5.2 N()

#### 5.2.1 Model

Exchangeable normal

\\\begin{align} \beta_j^{(m)} & \sim \text{N}\left(0, \tau_m^2 \right)
\\ \tau_m & \sim \text{N}^+\left(0, A\_{\tau}^{(m)2}\right)
\end{align}\\

#### 5.2.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{j=1}^{J_m} \text{N}(\beta_j^{(m)} \mid 0, \tau_m^2)
\end{equation}\\

#### 5.2.3 Forecasting

\\\begin{equation} \beta\_{J_m+h+1}^{(m)} \sim \text{N}(0, \tau_m^2)
\end{equation}\\

#### 5.2.4 Code

    N(s = 1)

- `s` is \\A\_{\tau}^{(m)}\\. Defaults to 1.

### 5.3 NFix()

#### 5.3.1 Model

Exchangeable normal, with fixed standard deviation

\\\begin{equation} \beta_j^{(m)} \sim \text{N}\left(0,
A\_{\beta}^{(m)2}\right) \end{equation}\\

#### 5.3.2 Contribution to posterior density

\\\begin{equation} \prod\_{j=1}^{J_m} \text{N}(\beta_j^{(m)} \mid 0,
A\_{\beta}^{(m)2}) \end{equation}\\

#### 5.3.3 Forecasting

\\\begin{equation} \beta\_{J_m+h+1}^{(m)} \sim \text{N}(0,
A\_{\beta}^{(m)2}) \end{equation}\\

#### 5.3.4 Code

    NFix(sd = 1)

- `sd` is \\A\_{\tau}^{(m)}\\. Defaults to 1.

### 5.4 RW()

#### 5.4.1 Model

Random walk

\\\begin{align} \beta\_{u,1}^{(m)} & \sim \text{N}\left(0,
(A_0^{(m)})^2\right) \\ \beta\_{u,v}^{(m)} & \sim
\text{N}(\beta\_{u,v-1}^{(m)}, \tau_m^2), \quad v = 2, \cdots, V_m \\
\tau_m & \sim \text{N}^+\left(0, (A\_{\tau}^{(m)})^2\right)
\end{align}\\

\\A_0^{(m)}\\ can be 0, implying that \\\beta\_{u,1}^{(m)}\\ is fixed at
0.

When \\U_m \> 1\\, constraints (Section [5.1.2](#sec:constraints)) can
be applied.

#### 5.4.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \text{N}\left(\beta\_{u,1}^{(m)} \mid 0,
(A_0^{(m)})^2\right) \prod\_{v=2}^{V_m} \text{N}\left(\beta\_{u,v}^{(m)}
\mid \beta\_{u,v-1}^{(m)}, \tau_m^2 \right) \end{equation}\\

#### 5.4.3 Forecasting

\\\begin{equation} \beta\_{u,V_m+h}^{(m)} \sim
\text{N}(\beta\_{u,V_m+h-1}^{(m)}, \tau_m^2) \end{equation}\\

If the prior includes sum-to-zero constraints, means are subtracted from
the forecasted values within each combination of ‘along’ and ‘by’
variables.

#### 5.4.4 Code

    RW(s = 1,
       along = NULL,
       con = c("none", "by"))

- `s` is \\A\_{\tau}^{(m)}\\. Defaults to 1.
- `sd` is \\A_0^{(m)}\\. Defaults to 1.
- `along` used to identify ‘along’ and ‘by’ dimensions.
- if `con` is `"by"`, sum-to-zero constraints are applied.

### 5.5 RW2()

#### 5.5.1 Model

Second-order random walk

\\\begin{align} \beta\_{u,1}^{(m)} & \sim \text{N}\left(0,
(A_0^{(m)})^2\right) \\ \beta\_{u,2}^{(m)} & \sim
\text{N}\left(\beta\_{u,1}, (A\_{\eta}^{(m)})^2\right) \\
\beta\_{u,v}^{(m)} & \sim \text{N}\left(2 \beta\_{u,v-1}^{(m)} -
\beta\_{u,v-2}^{(m)}, \tau_m^2\right), \quad v = 3, \cdots, V_m \\
\tau_m & \sim \text{N}^+\left(0, (A\_{\tau}^{(m)})^2\right)
\end{align}\\

\\A_0^{(m)}\\ can be 0, implying that \\\beta\_{u,1}^{(m)}\\ is fixed at
0.

When \\U_m \> 1\\, constraints (Section [5.1.2](#sec:constraints)) can
be applied.

#### 5.5.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \text{N}(\beta\_{u,1}^{(m)} \mid 0, (A_0^{(m)})^2)
\text{N}(\beta\_{u,2}^{(m)} \mid \beta\_{u,1}^{(m)},
(A\_{\eta}^{(m)})^2) \prod\_{v=3}^{V_m} \text{N}\left(\beta\_{u,v}^{(m)}
\mid 2 \beta\_{u,v-1}^{(m)} - \beta\_{u,v-2}^{(m)}, \tau_m^2 \right)
\end{equation}\\

#### 5.5.3 Forecasting

\\\begin{equation} \beta\_{u,V_m+h}^{(m)} \sim \text{N}(2
\beta\_{u,V_m+h-1}^{(m)} - \beta\_{u,V_m+h-2}^{(m)}, \tau_m^2)
\end{equation}\\

If the prior includes sum-to-zero constraints, means are subtracted from
the forecasted values within each combination of ‘along’ and ‘by’
variables.

#### 5.5.4 Code

    RW2(s = 1,
        sd = 1,
        sd_slope = 1,
        along = NULL,
        con = c("none", "by"))

- `s` is \\A\_{\tau}^{(m)}\\
- `sd` is \\A_0^{(m)}\\
- `sd_slope` is \\A\_{\eta}^{(m)}\\
- `along` used to identify ‘along’ and ‘by’ dimensions
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.6 RW2_Infant()

#### 5.6.1 Model

Second-order random walk with infant indicator. Designed for age
profiles for mortality rates. Along dimension must be age.

\\\begin{align} \beta\_{u,1}^{(m)} & \sim \text{N}(0, 1) \\
\beta\_{u,2}^{(m)} & \sim \text{N}\left(0, (A\_{\eta}^{(m)})^2\right) \\
\beta\_{u,3}^{(m)} & \sim \text{N}\left(2 \beta\_{u,2}^{(m)},
\tau_m^2\right) \\ \beta\_{u,v}^{(m)} & \sim \text{N}\left(2
\beta\_{u,v-1}^{(m)} - \beta\_{u,v-2}^{(m)}, \tau_m^2\right), \quad v =
4, \cdots, V_m \\ \tau_m & \sim \text{N}^+\left(0,
(A\_{\tau}^{(m)})^2\right) \end{align}\\

When \\U_m \> 1\\, constraints (Section [5.1.2](#sec:constraints)) can
be applied.

#### 5.6.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \text{N}(\beta\_{u,1}^{(m)} \mid 0, 1)
\text{N}(\beta\_{u,2}^{(m)} \mid 0, (A\_{\eta}^{(m)})^2)
\text{N}\left(\beta\_{u,3}^{(m)} \mid 2 \beta\_{u,2}^{(m)}, \tau_m^2
\right) \prod\_{v=4}^{V_m} \text{N}\left(\beta\_{u,v}^{(m)} \mid 2
\beta\_{u,v-1}^{(m)} - \beta\_{u,v-2}^{(m)}, \tau_m^2 \right)
\end{equation}\\

#### 5.6.3 Forecasting

Terms with an
[`RW2_Infant()`](https://bayesiandemography.github.io/bage/reference/RW2_Infant.md)
prior cannot be forecasted.

#### 5.6.4 Code

    RW2_Infant(s = 1,
               sd_slope = 1,
           con = c("none", "by"))

- `s` is \\A\_{\tau}^{(m)}\\
- `sd_slope` is \\A\_{\eta}^{(m)}\\
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.7 RW_Seas()

#### 5.7.1 Model

Random walk with seasonal effect

\\\begin{align} \beta\_{u,v}^{(m)} & = \alpha\_{u,v} + \lambda\_{u,v},
\quad v = 1, \cdots, V_m \\ \alpha\_{u,1}^{(m)} & \sim \text{N}\left(0,
(A_0^{(m)})^2 \right) \\ \alpha\_{u,v}^{(m)} & \sim
\text{N}(\alpha\_{u,v-1}^{(m)}, \tau_m^2), \quad v = 2, \cdots, V_m \\
\lambda\_{u,v}^{(m)} & \sim \text{N}(0, A\_{\lambda}^{(m)}), \quad v =
1, \cdots, S_m - 1 \\ \lambda\_{u,v}^{(m)} & = -\sum\_{s=1}^{S_m-1}
\lambda\_{u,v-s}^{(m)}, \quad v = S_m,\\ 2S_m, \cdots \\
\lambda\_{u,v}^{(m)} & \sim \text{N}(\lambda\_{u,v-S_m}^{(m)},
\omega_m^2), \quad \text{otherwise} \\ \tau_m & \sim \text{N}^+\left(0,
A\_{\tau}^{(m)2}\right) \\ \omega_m & \sim \text{N}^+\left(0,
A\_{\omega}^{(m)2}\right) \end{align}\\

\\A_0^{(m)}\\ can be 0, implying that \\\alpha\_{u,1}^{(m)}\\ is fixed
at 0.

\\A\_{\omega}^{(m)2}\\ can be set to zero, implying that seasonal
effects are fixed over time.

When \\U_m \> 1\\, constraints (Section [5.1.2](#sec:constraints)) can
be applied.

#### 5.7.2 Contribution to posterior density

\\\begin{align} & \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\text{N}(\omega_m \mid 0, A\_{\omega}^{(m)2}) \notag \\ & \quad \times
\prod\_{u=1}^{U_m} \bigg( \text{N}\left(\alpha\_{u,1}^{(m)} \mid 0,
(A_0^{(m)})^2 \right) \prod\_{v=2}^{V_m}
\text{N}\left(\alpha\_{u,v}^{(m)} \mid \alpha\_{u,v-1}^{(m)}, \tau_m^2
\right) \notag \\ & \quad \times \prod\_{v=1}^{S_m-1}
\text{N}\left(\lambda\_{u,v}^{(m)} \mid 0, (A\_{\lambda}^{(m)})^2\right)
\prod\_{\substack{v \> S_m \\ (v-1) \bmod S_m \neq 0}}^{V_m}
\text{N}\left(\lambda\_{u,v}^{(m)} \mid \lambda\_{u,v-S_m}^{(m)},
\omega_m^2\right) \bigg) \end{align}\\

#### 5.7.3 Forecasting

\\\begin{align} \alpha\_{J_m+h}^{(m)} & \sim
\text{N}(\alpha\_{J_m+h-1}^{(m)}, \tau_m^2) \\ \lambda\_{J_m+h}^{(m)} &
\sim \text{N}(\lambda\_{J_m+h-S_m}^{(m)}, \omega_m^2) \\
\beta\_{J_m+h}^{(m)} & = \alpha\_{J_m+h}^{(m)} + \lambda\_{J_m+h}^{(m)}
\end{align}\\

#### 5.7.4 Code

    RW_Seas(n_seas,
            s = 1,
        sd = 1,
        s_seas = 0,
        sd_seas = 1,
        along = NULL,
        con = c("none", "by"))

- `n_seas` is \\S_m\\
- `s` is \\A\_{\tau}^{(m)}\\
- `sd` is \\A_0^{(m)}\\
- `s_seas` is \\A\_{\omega}^{(m)}\\
- `sd_seas` is \\A\_{\lambda}^{(m)}\\
- `along` used to identify ‘along’ and ‘by’ dimensions
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.8 RW2_Seas()

#### 5.8.1 Model

Second-order random work, with seasonal effect

\\\begin{align} \beta\_{u,v}^{(m)} & = \alpha\_{u,v} + \lambda\_{u,v},
\quad v = 1, \cdots, V_m \\ \alpha\_{u,1}^{(m)} & \sim \text{N}\left(0,
(A_0^{(m)})^2 \right) \\ \alpha\_{u,2}^{(m)} & \sim
\text{N}\left(\alpha\_{u,1}^{(m)}, (A\_{\eta}^{(m)})^2\right) \\
\alpha\_{u,v}^{(m)} & \sim \text{N}(2 \alpha\_{u,v-1}^{(m)} -
\alpha\_{u,v-2}^{(m)}, \tau_m^2), \quad v = 3, \cdots, V_m \\
\lambda\_{u,v}^{(m)} & \sim \text{N}(0, A\_{\lambda}^{(m)}), \quad v =
1, \cdots, S_m - 1 \\ \lambda\_{u,v}^{(m)} & = -\sum\_{s=1}^{S_m-1}
\lambda\_{u,v}^{(m)}, \quad v = S_m,\\ 2S_m, \cdots \\
\lambda\_{u,v}^{(m)} & \sim \text{N}(\lambda\_{u,v-S_m}^{(m)},
\omega_m^2), \quad \text{otherwise} \\ \tau_m & \sim \text{N}^+\left(0,
A\_{\tau}^{(m)2}\right) \\ \omega_m & \sim \text{N}^+\left(0,
A\_{\omega}^{(m)2}\right) \end{align}\\

\\A_0^{(m)}\\ can be 0, implying that \\\alpha\_{u,1}^{(m)}\\ is fixed
at 0.

\\A\_{\omega}^{(m)2}\\ can be set to zero, implying that seasonal
effects are fixed over time.

When \\U_m \> 1\\, constraints (Section [5.1.2](#sec:constraints)) can
be applied.

#### 5.8.2 Contribution to posterior density

\\\begin{align} & \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\text{N}(\omega_m \mid 0, A\_{\omega}^{(m)2}) \notag \\ & \times
\prod\_{u=1}^{U_m} \bigg( \text{N}(\alpha\_{u,1}^{(m)} \mid 0,
(A_0^{(m)})^2 ) \text{N}(\alpha\_{u,2}^{(m)} \mid \alpha\_{u,1}^{(m)},
(A\_{\eta}^{(m)})^2 ) \notag \\ & \quad \times \prod\_{v=3}^{V_m}
\text{N}(\alpha\_{u,v}^{(m)} \mid 2 \alpha\_{u,v-1}^{(m)} -
\alpha\_{u,v-2}^{(m)}, \tau_m^2 ) \notag \\ & \quad \times
\prod\_{v=1}^{S_m-1} \text{N}(\lambda\_{u,v}^{(m)} \mid 0,
(A\_{\lambda}^{(m)})^2) \notag \\ & \quad \times \prod\_{\substack{v \>
S_m \\ (v-1) \bmod S_m \neq 0}}^{V_m} \text{N}(\lambda\_{u,v}^{(m)} \mid
\lambda\_{u,v-S_m}^{(m)}, \omega_m^2) \bigg) \end{align}\\

#### 5.8.3 Forecasting

\\\begin{align} \alpha\_{J_m+h}^{(m)} & \sim \text{N}(2
\alpha\_{J_m+h-1}^{(m)} - \alpha\_{J_m+h-2}^{(m)}, \tau_m^2) \\
\lambda\_{J_m+h}^{(m)} & \sim \text{N}(\lambda\_{J_m+h-S_m}^{(m)},
\omega_m^2) \\ \beta\_{J_m+h}^{(m)} & = \alpha\_{J_m+h}^{(m)} +
\lambda\_{J_m+h}^{(m)} \end{align}\\

#### 5.8.4 Code

    RW2_Seas(n_seas,
             s = 1,
         sd = 1,
         sd_slope = 1,
         s_seas = 0,
         sd_seas = 1,
         along = NULL,
         con = c("none", "by"))

- `n_seas` is \\S_m\\
- `s` is \\A\_{\tau}^{(m)}\\
- `sd` is \\A_0^{(m)}\\
- `sd_slope` is \\A\_{\eta}^{(m)}\\
- `s_seas` is \\A\_{\omega}^{(m)}\\
- `sd_seas` is \\A\_{\lambda}^{(m)}\\
- `along` used to identify ‘along’ and ‘by’ dimensions
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.9 AR()

#### 5.9.1 Model

\\\begin{equation} \beta\_{u,v}^{(m)} \sim \text{N}\left(\phi_1^{(m)}
\beta\_{u,v-1}^{(m)} + \cdots + \phi\_{K_m}^{(m)}
\beta\_{u,v-{K_m}}^{(m)}, \omega_m^2\right), \quad v = K_m + 1, \cdots,
V_m. \end{equation}\\ Internally, TMB derives values for
\\\beta\_{u,v}^{(m)}, v = 1, \cdots, K_m\\, and for \\\omega_m\\, that
imply a stationary distribution, and that give every term
\\\beta\_{u,v}^{(m)}\\ the same marginal variance. We denote this
marginal variance \\\tau_m^2\\, and assign it a prior \\\begin{equation}
\tau_m \sim \text{N}^+(0, A\_{\tau}^{(m)2}). \end{equation}\\ Each of
the \\\phi_k^{(m)}\\ has prior \\\begin{equation} \frac{\phi_k^{(m)} +
1}{2} \sim \text{Beta}(S_1^{(m)}, S_2^{(m)}). \end{equation}\\

#### 5.9.2 Contribution to posterior density

\\\begin{equation} \text{N}^+\left(\tau_m \mid 0, A\_{\tau}^{(m)2}
\right) \prod\_{k=1}^{K_m} \text{Beta}\left(\tfrac{1}{2} \phi_k^{(m)} +
\tfrac{1}{2} \mid 2, 2 \right) \prod\_{u=1}^{U_m} p\left(
\beta\_{u,1}^{(m)}, \cdots, \beta\_{u,V_m}^{(m)} \mid \phi_1^{(m)},
\cdots, \phi\_{K_m}^{(m)}, \tau_m \right) \end{equation}\\ where
\\p\left( \beta\_{u,1}^{(m)}, \cdots, \beta\_{u,V_m}^{(m)} \mid
\phi_1^{(m)}, \cdots, \phi\_{K_m}^{(m)}, \tau_m \right)\\ is calculated
internally by TMB.

#### 5.9.3 Forecasting

\\\begin{equation} \beta\_{u,V_m + h}^{(m)} \sim
\text{N}\left(\phi_1^{(m)} \beta\_{u,V_m + h - 1}^{(m)} + \cdots +
\phi\_{K_m}^{(m)} \beta\_{u,V_m+h-K_m}^{(m)}, \tau_m^2\right)
\end{equation}\\

#### 5.9.4 Code

    AR(n_coef = 2,
       s = 1,
       shape1 = 5,
       shape2 = 5,
       along = NULL,
       con = c("none", "by"))

- `n_coef` is \\K_m\\
- `s` is \\A\_{\tau}^{(m)}\\
- `shape1` is \\S_1^{(m)}\\
- `shape2` is \\S_2^{(m)}\\
- `along` is used to indentify the ‘along’ and ‘by’ dimensions

### 5.10 AR1()

Special case or
[`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md),
with extra options for autocorrelation coefficient.

#### 5.10.1 Model

\\\begin{align} \beta\_{u,1}^{(m)} & \sim \text{N}(0, \tau_m^2) \\
\beta\_{u,v}^{(m)} & \sim \text{N}(\phi_m \beta\_{u,v-1}^{(m)}, (1 -
\phi_m^2) \tau_m^2) \quad v = 2, \cdots, V_m \\ \phi_m & = a\_{0,m} +
(a\_{1,m} - a\_{0,m}) \phi_m^{\prime} \\ \phi_m^{\prime} & \sim
\text{Beta}(S_1^{(m)}, S_2^{(m)}) \\ \tau_m & \sim \text{N}^+\left(0,
A\_{\tau}^{(m)2}\right). \end{align}\\ This is adapted from the
specification used for AR1 densities in
[TMB](http://kaskr.github.io/adcomp/classdensity_1_1AR1__t.md). It
implies that the marginal variance of all \\\beta\_{u,v}^{(m)}\\ is
\\\tau_m^2\\. We require that \\-1 \< a\_{0m} \< a\_{1m} \< 1\\.

#### 5.10.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\text{Beta}( \phi_m^{\prime} \mid S_1^{(m)}, S_2^{(m)})
\prod\_{u=1}^{U_m} \text{N}\left(\beta\_{u,1}^{(m)} \mid 0, \tau_m^2
\right) \prod\_{u=1}^{U_m} \prod\_{j=2}^{V_m}
\text{N}\left(\beta\_{u,v}^{(m)} \mid \phi_m \beta\_{u,v-1}^{(m)}, (1 -
\phi_m^2) \tau_m^2 \right) \end{equation}\\

#### 5.10.3 Forecasting

\\\begin{equation} \beta\_{J_m + h}^{(m)} \sim \text{N}\left(\phi_m
\beta\_{J_m + h - 1}^{(m)}, (1 - \phi_m^2) \tau_m^2\right)
\end{equation}\\

#### 5.10.4 Code

    AR1(s = 1,
        shape1 = 5,
        shape2 = 5,
        min = 0.8,
        max = 0.98,
        along = NULL,
        con = c("none", "by"))

- `s` is \\A\_{\tau}^{(m)}\\
- `shape1` is \\S_1^{(m)}\\
- `shape2` is \\S_2^{(m)}\\
- `min` is \\a\_{0m}\\
- `max` is \\a\_{1m}\\
- `along` is used to identify ‘along’ and ‘by’ dimensions

The defaults for `min` and `max` are based on the defaults for function
`ets()` in R package **forecast** (Hyndman and Khandakar 2008).

### 5.11 Lin()

#### 5.11.1 Model

\\\begin{align} \beta\_{u,v}^{(m)} & = \alpha\_{u,v}^{(m)} +
\epsilon\_{u,v}^{(m)} \\ \alpha\_{u,v}^{(m)} & = \left(v - \frac{V_m +
1}{2}\right) \eta_u^{(m)} \\ \eta_u^{(m)} & \sim
\text{N}\left(B\_{\eta}^{(m)}, (A\_{\eta}^{(m)})^2\right)\\
\epsilon\_{u,v}^{(m)} & \sim \text{N}(0, \tau_m^2) \\ \tau_m & \sim
\text{N}^+\left(0, (A\_{\tau}^{(m)})^2\right) \end{align}\\

Note that \\\sum\_{v=1}^{V_m} \alpha\_{u,v}^{(m)} = 0\\.

Scale parameter \\A\_{\tau}^{(m)}\\ is allowed to equal 0, in which case
the model reduces to \\\begin{align} \beta\_{u,v}^{(m)} & = \left(v -
\frac{V_m + 1}{2}\right) \eta_u^{(m)} \\ \eta_u^{(m)} & \sim
\text{N}\left(B\_{\eta}^{(m)}, (A\_{\eta}^{(m)})^2\right) \end{align}\\

#### 5.11.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \text{N}(\eta_u^{(m)} \mid B\_{\eta}^{(m)},
A\_{\eta}^{(m)2}) \prod\_{u=1}^{U_m} \prod\_{v=1}^{V_m}
\text{N}\left(\beta\_{u,v}^{(m)} \\\middle\|\\ v - \frac{V_m + 1}{2},
\tau_m^2 \right) \end{equation}\\

When \\A\_{\tau}^{(m)} = 0\\, this reduces to \\\begin{equation}
\prod\_{u=1}^{U_m} \text{N}(\eta_u^{(m)} \mid B\_{\eta}^{(m)},
A\_{\eta}^{(m)2}) \end{equation}\\

#### 5.11.3 Forecasting

\\\begin{equation} \beta\_{u,V_m + h}^{(m)} \sim
\text{N}\left(\left(\frac{V_m - 1}{2}+ h\right) \eta_u^{(m)},
\tau_m^2\right) \end{equation}\\

When \\A\_{\tau}^{(m)} = 0\\, this reduces to \\\begin{equation}
\beta\_{u,V_m + h}^{(m)} = \left(\frac{V_m - 1}{2}+ h\right)
\eta_u^{(m)} \end{equation}\\

#### 5.11.4 Code

    Lin(s = 1,
        mean_slope = 0,
        sd_slope = 1,
        along = NULL,
        con = c("none", "by"))

- `s` is \\A\_{\tau}^{(m)}\\
- `mean_slope` is \\B\_{\eta}^{(m)}\\
- `sd_slope` is \\A\_{\eta}^{(m)}\\
- `along` is used to indentify ‘along’ and ‘by’ dimensions
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.12 Lin_AR()

#### 5.12.1 Model

\\\begin{align} \beta\_{u,v}^{(m)} & = \alpha\_{u,v}^{(m)} +
\epsilon\_{u,v}^{(m)} \\ \alpha\_{u,v}^{(m)} & = \left(v - \frac{V_m +
1}{2}\right) \eta_u^{(m)} \\ \eta_u^{(m)} & \sim
\text{N}\left(B\_{\eta}^{(m)}, (A\_{\eta}^{(m)})^2\right)\\
\epsilon\_{u,v}^{(m)} & \sim \text{N}\left(\phi_1^{(m)}
\epsilon\_{u,v-1}^{(m)} + \cdots + \phi\_{K_m}^{(m)}
\epsilon\_{u,v-{K_m}}^{(m)}, \omega_m^2\right), \quad v = K_m + 1,
\cdots, V_m \end{align}\\

Note that \\\sum\_{v=1}^{V_m} \alpha\_{u,v}^{(m)} = 0\\.

Internally, TMB derives values for \\\epsilon\_{u,v}^{(m)}, v = 1,
\cdots, K_m\\, and for \\\omega_m\\, that provide the
\\\epsilon\_{u,v}^{(m)}\\ with a stationary distribution in which each
term has the same marginal variance. We denote this marginal variance
\\\tau_m^2\\, and assign it a prior \\\begin{equation} \tau_m \sim
\text{N}^+(0, A\_{\tau}^{(m)2}). \end{equation}\\ Each of the individual
\\\phi_k^{(m)}\\ has prior \\\begin{equation} \frac{\phi_k^{(m)} + 1}{2}
\sim \text{Beta}(S_1^{(m)}, S_2^{(m)}). \end{equation}\\

#### 5.12.2 Contribution to posterior density

\\\begin{align} & \text{N}^+\left(\tau_m \mid 0, A\_{\tau}^{(m)2}
\right) \prod\_{k=1}^{K_m} \text{Beta}\left( \tfrac{1}{2} \phi_k^{(m)} +
\tfrac{1}{2} \mid S_1^{(m)}, S_2^{(m)} \right) \notag \\ & \quad \times
\prod\_{u=1}^{U_m} \text{N}(\eta_u^{(m)} \mid 0, A\_{\eta}^{(m)2})
p\left( \epsilon\_{u,1}^{(m)}, \cdots, \epsilon\_{u,V_m}^{(m)} \mid
\phi_1^{(m)}, \cdots, \phi\_{K_m}^{(m)}, \tau_m \right) \end{align}\\
where \\p\left( \epsilon\_{u,1}^{(m)}, \cdots, \epsilon\_{u,V_m}^{(m)}
\mid \phi_1^{(m)}, \cdots, \phi\_{K_m}^{(m)}, \tau_m \right)\\ is
calculated internally by TMB.

#### 5.12.3 Forecasting

\\\begin{align} \beta\_{u, V_m + h}^{(m)} & = \left(\frac{V_m - 1}{2}+
h\right) \eta_u^{(m)} + \epsilon\_{u,V_m+h}^{(m)} \\
\epsilon\_{u,V_m+h}^{(m)} & \sim \text{N}\left(\phi_1^{(m)}
\epsilon\_{u,V_m + h - 1}^{(m)} + \cdots + \phi\_{K_m}^{(m)}
\epsilon\_{u,V_m+h-K_m}^{(m)}, \omega_m^2\right) \end{align}\\

#### 5.12.4 Code

    Lin_AR(n_coef = 2,
           s = 1,
           shape1 = 5,
           shape2 = 5,
           mean_slope = 0,
           sd_slope = 1,
           along = NULL,
           con = c("none", "by"))

- `n_coef` is \\K_m\\
- `s` is \\A\_{\tau}^{(m)}\\
- `shape1` is \\S_1^{(m)}\\
- `shape2` is \\S_2^{(m)}\\
- `mean_slope` is \\B\_{\eta}^{(m)}\\
- `sd_slope` is \\A\_{\eta}^{(m)}\\
- `along` is used to indentify ‘along’ and ‘by’ variables
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.13 Lin_AR1()

#### 5.13.1 Model

\\\begin{align} \beta\_{u,v}^{(m)} & = \alpha\_{u,v}^{(m)} +
\epsilon\_{u,v}^{(m)} \\ \alpha\_{u,v}^{(m)} & = \left(v - \frac{V_m +
1}{2}\right) \eta_u^{(m)} \\ \eta_u^{(m)} & \sim
\text{N}\left(B\_{\eta}^{(m)}, (A\_{\eta}^{(m)})^2\right)\\
\epsilon\_{u,1}^{(m)} & \sim \text{N}\left(0, \tau_m^2 \right) \\
\epsilon\_{u,v}^{(m)} & \sim \text{N}\left(\phi_m
\epsilon\_{u,v-1}^{(m)}, (1 - \phi_m^2) \tau_m^2 \right), \quad v = 2,
\cdots, V_m \\ \phi_m & = a\_{0,m} + (a\_{1,m} - a\_{0,m})
\phi_m^{\prime} \\ \phi_m^{\prime} & \sim \text{Beta}(S_1^{(m)},
S_2^{(m)}) \\ \tau_m & \sim \text{N}^+\left(0, A\_{\tau}^{(m)2}\right).
\end{align}\\

Note that \\\sum\_{v=1}^{V_m} \alpha\_{u,v}^{(m)} = 0\\.

#### 5.13.2 Contribution to posterior density

\\\begin{align} & \text{N}^+\left(\tau_m \mid 0, A\_{\tau}^{(m)2}
\right) \text{Beta}( \phi_m^{\prime} \mid S_1^{(m)}, S_2^{(m)}) \notag
\\ & \quad \times \prod\_{u=1}^{U_m} \text{N}(\eta_u^{(m)} \mid 0,
A\_{\eta}^{(m)2}) \text{N}\left(\epsilon\_{u,1}^{(m)} \mid 0, \tau_m^2
\right) \prod\_{v=2}^{V_m} \text{N}\left(\epsilon\_{u,v}^{(m)} \mid
\phi_m \epsilon\_{u,v-1}^{(m)}, (1 - \phi_m^2) \tau_m^2 \right)
\end{align}\\

#### 5.13.3 Forecasting

\\\begin{align} \beta\_{u, V_m + h}^{(m)} & = \left(\frac{V_m - 1}{2}+
h\right) \eta_u^{(m)} + \epsilon\_{u,V_m+h}^{(m)} \\
\epsilon\_{u,V_m+h}^{(m)} & \sim \text{N}\left(\phi_m \epsilon\_{u,V_m +
h - 1}^{(m)}, (1 - \phi_m^2) \tau_m^2\right) \end{align}\\

#### 5.13.4 Code

    Lin_AR1(s = 1,
            shape1 = 5,
            shape2 = 5,
        min = 0.8,
        max = 0.98,
            mean_slope = 0,
            sd_slope = 1,
            along = NULL,
        con = c("none", "by"))

- `s` is \\A\_{\tau}^{(m)}\\
- `shape1` is \\S_1^{(m)}\\
- `shape2` is \\S_2^{(m)}\\
- `min` is \\a\_{0m}\\
- `max` is \\a\_{1m}\\
- `mean_slope` is \\B\_{\eta}^{(m)}\\
- `sd_slope` is \\A\_{\eta}^{(m)}\\
- `along` is used to indentify ‘along’ and ‘by’ variables
- if `con` is `"by"`, sum-to-zero constraints are applied

### 5.14 Sp()

#### 5.14.1 Model

Penalised spline (P-spline)

\\\begin{equation} \pmb{\beta}\_u^{(m)} = \pmb{B}^{(m)}
\pmb{\alpha}\_u^{(m)}, \quad u = 1, \cdots, U_m \end{equation}\\ where
\\\pmb{\beta}\_u^{(m)}\\ is the subvector of \\\pmb{\beta}^{(m)}\\
composed of elements from the \\u\\th combination of the ‘by’ variables,
\\\pmb{B}^{(m)}\\ is a \\V_m \times K_m\\ matrix of B-splines, and
\\\pmb{\alpha}\_u^{(m)}\\ has a second-order random walk prior (Section
[5.5](#sec:pr-rw2)).

\\\pmb{B}^{(m)} = (\pmb{b}\_1^{(m)}(\pmb{v}), \cdots,
\pmb{b}\_{K_m}^{(m)}(\pmb{v}))\\, with \\\pmb{v} = (1, \cdots,
V_m)^{\top}\\. The B-splines are centered, so that \\\pmb{1}^{\top}
\pmb{b}\_k^{(m)}(\pmb{v}) = 0\\, \\k = 1, \cdots, K_m\\.

#### 5.14.2 Contribution to posterior density

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \prod\_{k=1}^2 \text{N}(\alpha\_{u,k}^{(m)} \mid
0, 1) \prod\_{u=1}^{U_m}\prod\_{k=3}^{K_m}
\text{N}\left(\alpha\_{u,k}^{(m)} - 2 \alpha\_{u,k-1}^{(m)} +
\alpha\_{u,k-2}^{(m)} \mid 0, \tau_m^2 \right) \end{equation}\\

#### 5.14.3 Forecasting

Terms with a
[`Sp()`](https://bayesiandemography.github.io/bage/reference/Sp.md)
prior cannot be forecasted.

#### 5.14.4 Code

    Sp(n = NULL,
       s = 1)

- `n` is \\K_m\\. Defaults to \\\max(0.7 J_m, 4)\\.
- `s` is the \\A\_{\tau}^{(m)}\\ from the second-order random walk
  prior. Defaults to 1.
- `along` is used to identify ‘along’ and ‘by’ variables

### 5.15 SVD()

#### 5.15.1 Model

**Age but no sex or gender**

Let \\\pmb{\beta}\_u\\ be the age effect for the \\u\\th combination of
the ‘by’ variables. With an SVD prior, \\\begin{equation}
\pmb{\beta}\_u^{(m)} = \pmb{F}^{(m)} \pmb{\alpha}\_u^{(m)} +
\pmb{g}^{(m)}, \quad u = 1, \cdots, U_m \end{equation}\\ where
\\\pmb{F}^{(m)}\\ is a \\V_m \times K_m\\ matrix, and \\\pmb{g}^{(m)}\\
is a vector with \\V_m\\ elements, both derived from a singular value
decomposition (SVD) of an external dataset of age-specific values for
all sexes/genders combined. The construction of \\\pmb{F}^{(m)}\\ and
\\\pmb{g}^{(m)}\\ is described in Appendix [14.2](#app:svd). The
centering and scaling used in the construction allow use of the simple
prior \\\begin{equation} \alpha\_{u,k}^{(m)} \sim \text{N}(0, 1), \quad
u = 1, \cdots, U_m, k = 1, \cdots, K_m. \end{equation}\\

**Joint model of age and sex/gender**

In the joint model, vector \\\pmb{\beta}\_u\\ represents the interaction
between age and sex/gender for the \\u\\th combination of the ‘by’
variables. Matrix \\\pmb{F}^{(m)}\\ and vector \\\pmb{g}^{(m)}\\ are
calculated from data that separate sexes/genders. The model is otherwise
unchanged.

**Independent models for each sex/gender**

In the independent model, vector \\\pmb{\beta}\_{s,u}\\ represents age
effects for sex/gender \\s\\ and the \\u\\th combination of the ‘by’
variables, and we have \\\begin{equation} \pmb{\beta}\_{s,u}^{(m)} =
\pmb{F}\_s^{(m)} \pmb{\alpha}\_{s,u}^{(m)} + \pmb{g}\_s^{(m)}, \quad s =
1, \cdots, S; \quad u = 1, \cdots, U_m \end{equation}\\ Matrix
\\\pmb{F}\_s^{(m)}\\ and vector \\\pmb{g}\_s^{(m)}\\ are calculated from
data that separate sexes/genders. The prior is \\\begin{equation}
\alpha\_{s,u,k}^{(m)} \sim \text{N}(0, 1), \quad s = 1, \cdots, S; \quad
u = 1, \cdots, U_m; \quad k = 1, \cdots, K_m. \end{equation}\\

#### 5.15.2 Contribution to posterior density

\\\begin{equation} \prod\_{u=1}^{U_m}\prod\_{k=1}^{K_m}
\text{N}\left(\alpha\_{uk}^{(m)} \mid 0, 1 \right) \end{equation}\\ for
the age-only and joint models, and \\\begin{equation} \prod\_{s=1}^S
\prod\_{u=1}^{U_m}\prod\_{k=1}^{K_m} \text{N}\left(\alpha\_{s,u,k}^{(m)}
\mid 0, 1 \right) \end{equation}\\ for the independent model

#### 5.15.3 Forecasting

Terms with an SVD prior cannot be forecasted.

#### 5.15.4 Code

    SVD(ssvd,
        n_comp = NULL,
        indep = TRUE)

where - `ssvd` is an object containing \\\pmb{F}\\ and \\\pmb{g}\\ -
`n_comp` is the number of components to be used (which defaults to
`ceiling(n/2)`, where `n` is the number of components in `ssvd` -
`indep` determines whether and independent or joint model will be used
if the term being modelled contains a sex or gender variable.

### 5.16 SVD_RW()

#### 5.16.1 Model

The
[`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
prior is identical to the
[`SVD()`](https://bayesiandemography.github.io/bage/reference/SVD.md)
prior except that the coefficients evolve over time, following
independent random walks. For instance, in the combined-sex/gender and
joint models with \\K_m\\ SVD components,

\\\begin{align} \pmb{\beta}\_{u,t}^{(m)} & = \pmb{F}^{(m)}
\pmb{\alpha}\_{u,t}^{(m)} + \pmb{g}^{(m)} \\ \alpha\_{u,k,1}^{(m)} &
\sim \text{N}(0, (A_0^{(m)})^2), \\ \alpha\_{u,k,t}^{(m)} & \sim
\text{N}(\alpha\_{u,k,t-1}^{(m)}, \tau_m^2), \quad t = 2, \cdots, T \\
\tau_m & \sim \text{N}^+\left(0, (A\_{\tau}^{(m)})^2\right)
\end{align}\\

#### 5.16.2 Contribution to posterior density

In the combined-sex/gender and joint models,

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \prod\_{k=1}^{K_m} \text{N}(\alpha\_{u,k,1}^{(m)}
\mid 0, (A_0^{(m)})^2) \prod\_{t=2}^{T}
\text{N}\left(\alpha\_{u,k,t}^{(m)} \mid \alpha\_{u,k,t-1}^{(m)},
\tau_m^2 \right), \end{equation}\\

and in the independent model,

\\\begin{equation} \text{N}(\tau_m \mid 0, A\_{\tau}^{(m)2})
\prod\_{u=1}^{U_m} \prod\_{s=1}^{S} \prod\_{k=1}^{K_m}
\text{N}(\alpha\_{u,s,k,1}^{(m)} \mid 0, (A_0^{(m)})^2) \prod\_{t=2}^{T}
\text{N}\left(\alpha\_{u,s,k,t}^{(m)} \mid \alpha\_{u,s,k,t-1}^{(m)},
\tau_m^2 \right) \end{equation}\\

#### 5.16.3 Forecasting

\\\begin{align} \alpha\_{u,k,T+h}^{(m)} & \sim
\text{N}(\alpha\_{u,k,T+h-1}^{(m)}, \tau_m^2) \\
\pmb{\beta}\_{u,T+h}^{(m)} & = \pmb{F}^{(m)}
\pmb{\alpha}\_{u,T+h}^{(m)} + \pmb{g}^{(m)} \end{align}\\

#### 5.16.4 Code

    SVD_RW(ssvd,
           n_comp = NULL,
           indep = TRUE,
           s = 1,
           sd = 1,
           con = c("none", "by"))

where

- `ssvd` is an object containing \\\pmb{F}\\ and \\\pmb{g}\\
- `n_comp` is \\K_m\\
- `indep` determines whether and independent or joint model will be used
  if the term being modelled contains a sex or gender variable.
- `s` is \\A\_{\tau}^{(m)}\\
- `sd` is \\A_0^{(m)}\\

### 5.17 SVD_RW2(), SVD_AR(), SVD_AR1()

The
[`SVD_RW2()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md),
[`SVD_AR()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
and
[`SVD_AR1()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
priors have the same structure as the
[`SVD_RW()`](https://bayesiandemography.github.io/bage/reference/SVD_AR.md)
prior, but with
[`RW2()`](https://bayesiandemography.github.io/bage/reference/RW2.md),
[`AR()`](https://bayesiandemography.github.io/bage/reference/AR.md), and
[`AR1()`](https://bayesiandemography.github.io/bage/reference/AR1.md)
priors for the along dimension taking the place of the
[`RW()`](https://bayesiandemography.github.io/bage/reference/RW.md)
prior.

### 5.18 Known

#### 5.18.1 Model

Elements of \\\pmb{\beta}^{(m)}\\ are treated as known with certainty.

#### 5.18.2 Contribution to posterior density

Known priors make no contribution to the posterior density.

#### 5.18.3 Forecasting

Main effects with a known prior cannot be forecasted.

#### 5.18.4 Code

    Known(values)

- `values` is a vector containing the \\\beta_j^{(m)}\\.

## 6 Covariates

### 6.1 Model

Matrix \\\pmb{Z}\\ is a standardized version of the original covariate
data supplied by the user. We standardize every numeric variable to have
mean 0 and standard deviation one. We then convert categorical variables
to sets of indicator variables using R’s ‘treatment’ contrast. If a
variable in the original data is categorical with \\C\\ categories, then
it is converted into \\C-1\\ indicator variables, with the first
category as the omitted variable.

The elements of \\\pmb{\zeta}\\ have prior \\\begin{equation} \zeta_p
\sim \text{N}(0, 1) \end{equation}\\

### 6.2 Contribution to posterior density

\\\begin{equation} \prod\_{p=1}^P \text{N}(\zeta_p \| 0, 1)
\end{equation}\\

### 6.3 Forecasting

A model with covariates can be used for forecasting provided that

- the coefficients (the \\\zeta_p\\) are non-time-varying,
- future values for the covariates (the columns of \\\pmb{Z}\\) can be
  inferred from the classifying variables (other than time), or are
  supplied by the user.

### 6.4 Code

    set_covariates(mod, formula)

- `mod` Object of class `"bage_mod"`
- `formula` One-sided R formula describing the covariates to be used

## 7 Prior for dispersion terms

### 7.1 Model

Use exponential distribution, parameterised using mean,
\\\begin{equation} \xi \sim \text{Exp}(\mu\_{\xi}) \end{equation}\\

### 7.2 Contribution to prior density

\\\begin{equation} p(\xi) = \frac{1}{\mu\_{\xi}}
\exp\left(\frac{-\xi}{\mu\_{\xi}}\right) \end{equation}\\

### 7.3 Code

    set_disp(mean = 1)

- `mean` is \\\mu\_{\xi}\\

## 8 Data models

### 8.1 Overview

Principles:

- Can have data model for outcome, or for exposure, but not both
- But *can* have data model and confidentialization (eg model of
  undercount, combined with RR3)
- Models must scale well – cannot use direct approach of treating true
  outcome, or true exposure, as parameter to be estimated directly
- Poisson cannot include dispersion terms for rates, and binomial models
  cannot include dispersion terms for probabilities

Notation:

- When modelling errors in outcome, distinguish between
  \\y^{\text{true}}\\ and \\y^{\text{obs}}\\. When modelling errors in
  exposure, distinguish between \\w^{\text{true}}\\ and
  \\w^{\text{obs}}\\.

| Model               |                                                        System model                                                         |                                                                                                                                  Data model                                                                                                                                  |
|:--------------------|:---------------------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| Poisson undercount  |     \\ y_i^{\text{true}} \sim \text{Poisson}(\gamma_i w_i) \\ \gamma_i \sim \text{Gamma}(\xi^{-1}, (\xi \mu_i)^{-1}) \\     |                                                                              \\ y_i^{\text{obs}} \sim \text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]}) \\ \pi_g \sim \text{Beta}(a_g, b_g) \\                                                                               |
| Poisson overcount   |     \\ y_i^{\text{true}} \sim \text{Poisson}(\gamma_i w_i) \\ \gamma_i \sim \text{Gamma}(\xi^{-1}, (\xi \mu_i)^{-1}) \\     |                                                       \\ y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i \\ \epsilon_i \sim \text{Poisson}(\kappa\_{g\[i\]} \gamma_i w_i) \\ \kappa_g \sim \text{Gamma}(a_g, b_g) \\                                                       |
| Poisson miscount    |     \\ y_i^{\text{true}} \sim \text{Poisson}(\gamma_i w_i) \\ \gamma_i \sim \text{Gamma}(\xi^{-1}, (\xi \mu_i)^{-1}) \\     | \\ y_i^{\text{obs}} = u_i + v_i \\ u_i \sim \text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]}) \\ v_i \sim \text{Poisson}(\kappa\_{h\[i\]} \gamma_i w_i) \\ \pi_g \sim \text{Beta}(a_g^{(\pi)}, b_g^{(\pi)}) \\ \kappa_h \sim \text{Gamma}(a_h^{(\kappa)}, b_h^{(\kappa)}) \\ |
| Poisson noise       |                                   \\ y_i^{\text{true}} \sim \text{Poisson}(\mu_i w_i) \\                                    |                                                                             \\ y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i \\ \epsilon_i \sim \text{Skellam}(m\_{g\[i\]}, m\_{g\[i\]}) \\                                                                              |
| Poisson exposure    |                                   \\ y_i \sim \text{Poisson}(\mu_i w_i^{\text{true}}) \\                                    |                                                                                \\ w_i^{\text{obs}} \sim \text{InvGamma}(2 + d\_{g\[i\]}^{-1}, \[1 + d\_{g\[i\]}^{-1}\] w_i^{\text{true}}) \\                                                                                 |
| Binomial undercount | \\ y_i^{\text{true}} \sim \text{Binomial}(w_i, \gamma_i) \\ \gamma_i \sim \text{Beta}(\mu_i \xi^{-1}, (1-\mu_i)\xi^{-1}) \\ |                                                                              \\ y_i^{\text{obs}} \sim \text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]}) \\ \pi_g \sim \text{Beta}(a_g, b_g) \\                                                                               |
| Normal noise        |                             \\ y_i^{\text{true}} \sim \text{N}(\gamma_i, w_i^{-1} \sigma^2) \\                              |                                                                                    \\ y_i^{\text{obs}} = y_i^{\text{true}} + \epsilon_i \\ \epsilon_i \sim \text{N}(0, s\_{g\[i\]}^2) \\                                                                                     |

Table 8.1: System models and data models

### 8.2 Poisson with undercount in outcome variable

#### 8.2.1 System model

\\\begin{align} y_i^{\text{true}} & \sim \text{Poisson}(\gamma_i w_i)
\tag{8.1} \\ \gamma_i & \sim \text{Gamma}(\xi^{-1}, (\mu_i \xi)^{-1})
\end{align}\\ with possibility that \\\xi \equiv 0\\ so that
\\\begin{equation} y_i^{\text{true}} \sim \text{Poisson}(\mu_i w_i)
\end{equation}\\

#### 8.2.2 Data model

\\\begin{align} y_i^{\text{obs}} & \sim
\text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]}) \tag{8.2} \\ \pi_g &
\sim \text{Beta}(a_g, b_g) \\ \end{align}\\ where values for \\a_g\\,
\\b_g\\ are supplied by the user.

#### 8.2.3 Combined model seen by TMB

\\\begin{align} y_i^{\text{obs}} & \sim \begin{cases}
\text{NegBinom}\left(\xi^{-1}, \[1 + \pi_i \mu_i w_i \xi\]^{-1}\right) &
\xi \> 0 \\ \text{Poisson}(\pi_i \mu_i w_i) & \xi \equiv 0 \end{cases}
\\ z_g & \sim \text{N}(0, 1) \\ u_g & = F\_{\text{N}(0,1)}(z_g) \\ \pi_g
& = F\_{\text{Beta}(a_g, b_g)}^{-1}(u_g) \end{align}\\

We use the normally-distributed latent variable \\z_g\\ and then
transform to calculate the density for \\\pi_g\\ because TMB is only
able to safely integrate out normally-distributed latent variables.

#### 8.2.4 Obtaining \\y_i^{\text{true}}\\

Draw \\\begin{equation} x_i = y_i^{\text{true}} - y_i^{\text{obs}} \sim
\begin{cases} \text{NegBinom}\left(y_i^{\text{obs}} + \xi^{-1},
\frac{1 + \pi\_{g\[i\]} \mu_i w_i \xi}{1 + \mu_i w_i \xi} \right) & \xi
\> 0 \\ \text{Poisson}(\[1-\pi\_{g\[i\]}\] \mu_i w_i) & \xi \equiv 0
\end{cases} \end{equation}\\ then set \\y_i^{\text{true}} =
y_i^{\text{obs}} + x_i\\.

Appendix [14.3](#app:datamod) has the derivation.

### 8.3 Poisson with overcount in outcome variable

#### 8.3.1 System model

\\\begin{align} y_i^{\text{true}} & \sim \text{Poisson}(\gamma_i w_i)
\tag{8.1} \\ \gamma_i & \sim \text{Gamma}(\xi^{-1}, (\mu_i \xi)^{-1})
\end{align}\\ with possibility that \\\xi \equiv 0\\ so that
\\\begin{equation} y_i^{\text{true}} \sim \text{Poisson}(\mu_i w_i)
\end{equation}\\

#### 8.3.2 Data model

\\\begin{align} y_i^{\text{obs}} & = y_i^{\text{true}} + \epsilon_i \\
\epsilon_i & \sim \text{Poisson}(\kappa\_{g\[i\]} \gamma_i w_i)
\tag{8.3} \\ \kappa_g & \sim \text{Gamma}(a_g, b_g) \end{align}\\ where
values for \\a_g\\, \\b_g\\ are supplied by the user.

#### 8.3.3 Combined model seen by TMB

\\\begin{align} y_i^{\text{obs}} & \sim \begin{cases}
\text{NegBinom}\left(\xi^{-1}, \[1 + \kappa\_{g\[i\]}\]\mu_i w_i
\xi\]^{-1}\right) & \xi \> 0 \\ \text{Poisson}(\[1 +
\kappa\_{g\[i\]}\]\mu_i w_i) & \xi \equiv 0 \end{cases} \\ z_g & \sim
\text{N}(0, 1) \\ u_g & = F\_{\text{N}(0,1)}(z_g) \\ \kappa_g & =
F\_{\text{Gamma}(a_g, b_g)}^{-1}(u_g) \end{align}\\

We use the normally-distributed latent variable \\z_g\\ and then
transform to calculate the density for \\\kappa_g\\ because TMB is only
able to safely integrate out normally-distributed latent variables.

#### 8.3.4 Obtaining \\y_i^{\text{true}}\\

\\\begin{equation} y_i^{\text{true}} \mid y_i^{\text{obs}},
\kappa\_{g\[i\]}, \mu_i, \xi, w_i \sim
\text{Binomial}\left(y_i^{\text{obs}}, \frac{1}{1 +
\kappa\_{g\[i\]}}\right) \end{equation}\\

Appendix [14.3](#app:datamod) has the derivation.

### 8.4 Poisson with undercount and overcount in outcome variable

#### 8.4.1 System model

\\\begin{align} y_i^{\text{true}} & \sim \text{Poisson}(\gamma_i w_i)
\tag{8.1} \\ \gamma_i & \sim \text{Gamma}(\xi^{-1}, (\mu_i \xi)^{-1})
\end{align}\\ with possibility that \\\xi \equiv 0\\ so that
\\\begin{equation} y_i^{\text{true}} \sim \text{Poisson}(\mu_i w_i)
\end{equation}\\

#### 8.4.2 Data model

\\\begin{align} y_i^{\text{obs}} & = u_i + v_i \\ u_i & \sim
\text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]}) \tag{8.2} \\ v_i &
\sim \text{Poisson}(\kappa\_{h\[i\]} \gamma_i w_i) \\ \pi_g & \sim
\text{Beta}(a_g^{(\pi)}, b_g^{(\pi)}) \\ \kappa_h & \sim
\text{Gamma}(a_h^{(\kappa)}, b_h^{(\kappa)}), \end{align}\\ where values
for \\a_g^{(\pi)}\\, \\b_g^{(\pi)}\\, \\a_h^{(\kappa)}\\, and
\\b_h^{(\kappa)}\\ are supplied by the user.

#### 8.4.3 Combined model seen by TMB

\\\begin{align} y_i^{\text{obs}} & \sim \begin{cases}
\text{NegBinom}\left(\xi^{-1}, \[1 + (\pi\_{g\[i\]} +
\kappa\_{h\[i\]})\mu_i w_i \xi\]^{-1}\right) & \xi \> 0 \\
\text{Poisson}(\[\pi\_{g\[i\]} + \kappa\_{h\[i\]}\]\mu_i w_i) & \xi
\equiv 0 \end{cases} \\ z_g^{(\pi)} & \sim \text{N}(0, 1) \\ u_g^{(\pi)}
& = F\_{\text{N}(0,1)}(z_g^{(\pi)}) \\ \pi_g & =
F\_{\text{Beta}(a_g^{(\pi)}, b_g^{(\pi)})}^{-1}(u_g^{(\pi)}) \\
z_h^{(\kappa)} & \sim \text{N}(0, 1) \\ u_h^{(\kappa)} & =
F\_{\text{N}(0,1)}(z_h^{(\kappa)}) \\ \kappa_h & =
F\_{\text{Gamma}(a_h^{(\kappa)}, b_h^{(\kappa)})}^{-1}(u_h^{(\kappa)})
\end{align}\\

#### 8.4.4 Obtaining \\y_i^{\text{true}}\\

As shown in Appendix [14.3](#app:datamod), \\\begin{equation} u_i \mid
y_i^{\text{obs}}, \pi\_{g\[i\]}, \kappa_i, \mu_i, \xi, w_i \sim
\text{Binomial}\left(y_i^{\text{obs}},
\frac{\pi\_{g\[i\]}}{\pi\_{g\[i\]} + \kappa_i}\right) \end{equation}\\

As also shown in Appendix [14.3](#app:datamod), \\\begin{equation}
y_i^{\text{true}} - u_i \| u_i, y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i,
\xi, w_i \sim \begin{cases} \text{NegBinom}\left(u_i + \xi^{-1},
\frac{1 + \pi\_{g\[i\]} \mu_i w_i \xi}{1 + \mu_i w_i \xi} \right) & \xi
\> 0 \\ \text{Poisson}(\[1-\pi\_{g\[i\]}\]\mu_i w_i) & \xi \equiv 0
\end{cases} \end{equation}\\

We can obtain a value for \\y_i^{\text{true}}\\ by drawing a value for
\\u_i\\, then drawing a value for \\x_i = y_i^{\text{true}} - u_i\\,
then setting \\y_i^{\text{true}} = x_i + u_i\\.

### 8.5 Poisson with noise added to outcome variable

#### 8.5.1 System model

Dispersion in rates not permitted.

\\\begin{equation} y_i^{\text{true}} \sim \text{Poisson}(\mu_i w_i)
\end{equation}\\

#### 8.5.2 Data model

\\\begin{align} y_i^{\text{obs}} & = y_i^{\text{true}} + \epsilon_i \\
\epsilon_i & \sim \text{Skellam}(m\_{g\[i\]}, m\_{g\[i\]}) \end{align}\\
where values for \\m_g\\ are supplied by the user.

#### 8.5.3 Combined model seen by TMB

\\\begin{equation} y_i^{\text{obs}} \sim \text{Skellam}(\mu_i w_i +
m\_{g\[i\]}, m\_{g\[i\]}) \end{equation}\\

#### 8.5.4 Obtaining \\y_i^{\text{true}}\\

\\\begin{equation} p(y_i^{\text{true}} \mid y_i^{\text{obs}},
m\_{g\[i\]}, \mu_i, w_i) \propto \frac{(\mu_i
w_i)^{y_i^{\text{true}}}}{y_i^{\text{true}}!} \\ I\_{\lvert
y_i^{\text{obs}} - y_i^{\text{true}} \rvert}(2 m\_{g\[i\]})
\end{equation}\\ where \\I\_{\lvert \nu \rvert}(x)\\ is a Bessel
function of the first kind. Derivation in Appendix [14.3](#app:datamod).

We use the exact distribution when \\\mu_i w_i \< 50\\ or \\m\_{g\[i\]}
\< 50\\, and a normal approximation otherwise.

### 8.6 Poisson with measurement errors in exposure

#### 8.6.1 System model

Drop gamma errors for rates, and be explicit that outcome depends on
actual exposure: \\\begin{equation} y_i \sim \text{Poisson}(\mu_i
w_i^{\text{true}}) \tag{8.4} \end{equation}\\

#### 8.6.2 Data model

\\\begin{equation} w_i^{\text{obs}} \sim \text{InvGamma}\left(2 +
d\_{g\[i\]}^{-1}, \[1 + d\_{g\[i\]}^{-1}\] w_i^{\text{true}}\right)
\end{equation}\\ where values for \\d_g\\ are supplied by the user.

Note that \\\begin{align} E\[w_i^{\text{obs}} \mid w_i^{\text{true}}\] &
= w_i^{\text{true}} \\ \text{var}\[w_i^{\text{obs}} \mid
w_i^{\text{true}}\] & = d\_{g\[i\]} ( w_i^{\text{true}})^2 \\
w_i^{\text{true}} \mid w_i^{\text{obs}} & \sim \text{Gamma}\left(3 +
d\_{g\[i\]}^{-1}, \frac{1 + d\_{g\[i\]}^{-1}}{w_i^{\text{obs}}}\right)
\tag{8.5} \\ E\[w_i^{\text{true}} \mid w_i^{\text{obs}}\] & =
\frac{3d\_{g\[i\]} + 1}{d\_{g\[i\]} + 1} w_i^{\text{obs}} \end{align}\\

[(8.5)](#eq:w-true) is derived in Appendix [14.3](#app:datamod)

#### 8.6.3 Combined model seen by TMB

\\\begin{equation} y_i^{\text{obs}} \sim \text{NegBinom}\left(3 +
d\_{g\[i\]}^{-1}, \frac{1 + d\_{g\[i\]}^{-1}}{1 + d\_{g\[i\]}^{-1} +
\mu_i w_i^{\text{obs}}} \right) \end{equation}\\

Note that \\\begin{equation} E\[y_i^{\text{obs}}\] = \left( \frac{3 +
d\_{g\[i\]}^{-1}}{1 + d\_{g\[i\]}^{-1}} \right) \mu_i w_i^{\text{obs}}
\end{equation}\\

#### 8.6.4 Obtaining \\w_i^{\text{true}}\\

Equations [(8.4)](#eq:lik-pois-offset) and [(8.5)](#eq:w-true) imply
\\\begin{equation} w_i^{\text{true}} \sim \text{Gamma}\left(3 +
d\_{g\[i\]}^{-1} + y_i, \frac{1 + d\_{g\[i\]}^{-1}}{w_i^{\text{obs}}} +
\mu_i \right) \end{equation}\\

Note that \\\begin{align} E\[w_i^{\text{true}}\] & = \frac{(3 +
d\_{g\[i\]}^{-1} + y_i) w_i^{\text{obs}}}{1 + d\_{g\[i\]}^{-1} + \mu_i
w_i^{\text{obs}}} \\ & = \lambda_i \hat{w}\_i^{\text{data}} + (1 -
\lambda_i) \hat{w}\_i^{\text{rate}} \end{align}\\ where \\\begin{align}
\hat{w}\_i^{\text{data}} & =\frac{3 + d\_{g\[i\]}^{-1}}{1 +
d\_{g\[i\]}^{-1}} w_i^{\text{obs}} \\ \hat{w}\_i^{\text{data}} & =
\frac{y_i}{\mu_i} \\ \lambda_i & = \frac{1 + d\_{g\[i\]}^{-1}}{1 +
d\_{g\[i\]}^{-1} + \mu_i w_i^{\text{obs}}}. \end{align}\\

\\\hat{w}\_i^{\text{data}}\\ is an estimate of \\w_i^{\text{true}}\\
derived from the data model, and \\\hat{w}\_i^{\text{rate}}\\ is an
estimate derived from the model for the rates.

### 8.7 Binomial with measurement errors in outcome variable

#### 8.7.1 System model

Same as original model, but be explicit that modelling actual outcome:
\\\begin{align} y_i^{\text{true}} & \sim \text{Binom}(w_i, \gamma_i)
\tag{8.6} \\ \gamma_i & \sim \text{Beta}\left(\xi^{-1}\mu_i, \xi^{-1}
(1-\mu_i) \right), \tag{8.7} \end{align}\\ with option that \\\xi \equiv
0\\ so that \\\begin{equation} y_i^{\text{true}} \sim
\text{Binomial}(w_i, \mu_i). \tag{8.8} \end{equation}\\

#### 8.7.2 Data model

\\\begin{align} y_i^{\text{obs}} & \sim
\text{Binomial}(y_i^{\text{true}}, \pi\_{g\[i\]}) \tag{8.9} \\ \pi_g &
\sim \text{Beta}(A_g^{(\pi)}, B_g^{(\pi)}) \end{align}\\ where values
for \\A_g^{(\pi)}\\ and \\B_g^{(\pi)}\\ are supplied by the user.

#### 8.7.3 Combined model seen by TMB

\\\begin{align} y_i^{\text{obs}} & \sim \begin{cases}
\text{BetaBinom}(w_i, \xi^{-1} \pi\_{g\[i\]} \mu_i, \xi^{-1}
\[1-\pi\_{g\[i\]} \mu_i\]) & \xi \> 0 \\ \text{Binomial}(w_i,
\pi\_{g\[i\]} \mu_i) & \xi \equiv 0 \end{cases} \\ \pi_g & \sim
\text{Beta}(A_g^{(\pi)}, B_g^{(\pi)}) \end{align}\\

#### 8.7.4 Obtaining \\y_i^{\text{true}}\\

When \\\xi \> 0\\, draw from distribution defined by \\\begin{align} &
p(y_i^{\text{true}} \mid y_i^{\text{obs}}, \mu_i, \xi, w_i,
\pi\_{g\[i\]}) \\ & \propto
\frac{1}{(y_i^{\text{true}}-y_i^{\text{obs}})!
(w_i-y_i^{\text{true}})!}\\ (1-\pi\_{g\[i\]})^{y_i^{\text{true}}}
B(y_i^{\text{true}}+\xi^{-1}\mu_i,
w_i-y_i^{\text{true}}+\xi^{-1}\[1-\mu_i\]) \end{align}\\

When \\\xi \equiv 0\\, draw \\\begin{equation} x_i = y_i^{\text{true}} -
y_i^{\text{obs}} \sim \text{Binomial}\left(w_i - y_i^{\text{obs}},
\frac{\[1 - \pi\_{g\[i\]}\] \mu_i}{1-\pi\_{g\[i\]}\mu_i} \right),
\end{equation}\\ then set \\y_i^{\text{true}} = y_i^{\text{obs}} +
x_i\\.

Derivation in Appendix [14.3](#app:datamod).

### 8.8 Normal with noise added to outcome variable

#### 8.8.1 System model

Same as original model, but be explicit that modelling actual outcome,
and scale \\\mu\\ and \\\xi\\ by values calculated from reported rather
than actual \\y_i\\: \\\begin{align} y_i^{\text{true}} & \sim
\text{N}(\gamma_i, w_i^{-1} \sigma^2) \\ \gamma_i & = \bar{y} + s \mu_i
\\ \sigma^2 & = \bar{w} s^2 \xi^2 \end{align}\\ where \\\begin{align}
\bar{y} & = \frac{\sum\_{i=1}^n y_i^{\text{obs}}}{n} \\ s & =
\sqrt{\frac{\sum\_{i=1}^n (y_i^{\text{obs}} - \bar{y})^2}{n-1}} \\
\bar{w} & = \frac{\sum\_{i=1}^n w_i}{n}. \end{align}\\

#### 8.8.2 Data model

\\\begin{equation} y_i^{\text{obs}} \sim \text{N}(y_i^{\text{true}},
s\_{h\[i\]}^2) \end{equation}\\ where values for \\s\_{h\[i\]}\\ are
supplied by the user.

#### 8.8.3 Combined model seen by TMB

\\\begin{equation} \frac{y_i^{\text{obs}} - \bar{y}}{s} \sim
\text{N}\left(\mu_i, \frac{\bar{w} \xi^2}{w_i} +
\frac{s\_{h\[i\]}^2}{s^2}\right) \end{equation}\\

#### 8.8.4 Obtaining \\y_i^{\text{true}}\\

\\\begin{equation} y_i^{\text{true}} \sim
\text{N}\left(\frac{\tau_1}{\tau_1 + \tau_2} \gamma_i +
\frac{\tau_2}{\tau_1 + \tau_2} y_i^{\text{obs}},\quad \frac{1}{\tau_1 +
\tau_2} \right) \end{equation}\\ where \\\tau_1 = w_i \sigma^{-2}\\ and
\\\tau_2 = s\_{h\[i\]}^{-2}\\

## 9 Confidentialization

### 9.1 Random Rounding to Multiples of 3

#### 9.1.1 Procedure

Random rounding to multiples of 3 (RR3) is applied to counts data. Let
\\y_i^{\text{un}}\\ denote the original unconfidentialized count, and
\\y_i^{\text{con}}\\ the confidentialised version. Then, under RR3,

- If \\y_i^{\text{un}} \bmod 3 = 0\\, then \\y_i^{\text{con}} =
  y_i^{\text{un}}\\.
- if \\y_i^{\text{un}} \bmod 3 = 1\\ then \\y_i^{\text{con}} =
  y_i^{\text{un}}-1\\ with probability 2/3, and \\y_i^{\text{con}} =
  y_i^{\text{un}} + 2\\ with probability 1/3;
- if \\y_i^{\text{un}} \bmod 3 = 2\\ then \\y_i^{\text{con}} =
  y_i^{\text{un}}-2\\ with probability 1/3, and \\y_i^{\text{con}} =
  y_i^{\text{un}} + 1\\ with probability 2/3.

#### 9.1.2 Model seen by TMB

\\\begin{align} p\_{\text{con}}(y_i^{\text{con}}) & =
\sum\_{y_i^{\text{un}}} p\_{\text{con}\|\text{un}}(y_i^{\text{con}} \|
y_i^{\text{un}}) p\_{\text{un}}(y_i^{\text{un}}) \\ & = \sum\_{k = -2}^2
p\_{\text{con}\|\text{un}}(y_i^{\text{con}} \| y_i^{\text{con}} + k)
p\_{\text{un}}(y_i^{\text{con}} + k) \end{align}\\ where

- \\p\_{\text{con}\|\text{un}}(\cdot) = \begin{cases} 1 & \text{ if } \|
  y_i^{\text{con}} - y_i^{\text{un}} \| = 0 \\ \frac{2}{3} & \text{ if }
  \| y_i^{\text{con}} - y_i^{\text{un}} \| = 1 \\ \frac{1}{3} & \text{
  if } \| y_i^{\text{con}} - y_i^{\text{un}} \| = 2 \end{cases}\\

and

- \\p\_{\text{un}}(\cdot) = \text{NegBinom}\left(\cdot \mid \xi^{-1},
  \[1 + \mu_i w_i \xi\]^{-1}\right)\\ in Poisson model with no
  measurement errors;
- \\p\_{\text{un}}(\cdot) = \text{NegBinom}\left(\cdot \mid \xi^{-1},
  \[1 + \pi\_{g\[i\]} \mu_i w_i \xi\]^{-1}\right)\\ in Poisson model
  with undercount in outcome;
- \\p\_{\text{un}}(\cdot) = \text{NegBinom}\left(\cdot \mid \xi^{-1},
  \[1 + (1 + \kappa\_{g\[i\]}) \mu_i w_i \xi\]^{-1}\right)\\ in Poisson
  model with overcount in outcome;
- \\p\_{\text{un}}(\cdot) = \text{NegBinom}\left(\cdot \mid \xi^{-1},
  \[1 + (\pi\_{g\[i\]} + \kappa\_{h\[i\]}) \mu_i w_i \xi\]^{-1}\right)\\
  in Poisson model with miscount in outcome;
- \\p\_{\text{un}}(\cdot) = \text{Skellam}\left(\cdot \mid \mu_i w_i +
  m\_{g\[i\]}, m\_{g\[i\]} \right)\\ in Poisson model with noise added
  to outcome;
- \\p\_{\text{un}}(\cdot) = \text{NegBinom}\left(\cdot \mid 3 +
  d\_{g\[i\]}^{-1}, \[1 + (1 + d\_{g\[i\]}^{-1})^{-1} \mu_i
  w_i^{\text{obs}}\]^{-1} \right)\\ in Poisson model with measurement
  error in exposure;
- \\p\_{\text{un}}(\cdot) = \text{BetaBinomial}(\cdot \mid w_i, \xi^{-1}
  \mu_i, \xi^{-1} \[1-\mu_i\])\\ in Binomial model with no measurement
  error; and
- \\p\_{\text{un}}(\cdot) = \text{BetaBinomial}(\cdot \mid w_i, \xi^{-1}
  \pi\_{g\[i\]} \mu_i, \xi^{-1} \[1-\pi\_{g\[i\]} \mu_i\])\\ in Binomial
  model with measurement error in outcome.

#### 9.1.3 Deriving \\y_i^{\text{un}}\\

Draw from \\\begin{equation} p\_{\text{un}\|\text{con}}(y_i^{\text{un}}
\mid y_i^{\text{con}}) \propto
p\_{\text{con}\|\text{un}}(y_i^{\text{con}} \mid y_i^{\text{un}})
p\_{\text{un}}(y_i^{\text{un}}) \end{equation}\\ where
\\p\_{\text{con}\|\text{un}}(\cdot)\\ and \\p\_{\text{un}}(\cdot)\\ are
defined as in Section [9.1.2](#sec:confid-tmb).

## 10 Estimation

### 10.1 Filtering

The data that we supply to TMB is a a filtered version of the data that
the user provides through the `data` argument. We remove any rows where
(i) the offset is 0 or NA, or (ii) the outcome variable is NA.

### 10.2 Inner-Outer Approximation

The inner-outer approximation cannot be used with models that include
data models.

#### 10.2.1 Step 0: Select ‘inner’ and ‘outer’ variables

Select variables to be used in inner model. By default, these are the
age, sex, and time variables in the model. All remaining variables are
‘outer’ variables. Any covariates specified through
[`set_covariates()`](https://bayesiandemography.github.io/bage/reference/set_covariates.md)
are also treated as ‘outer’ variables.

#### 10.2.2 Step 1: Fit inner model

In Poisson and binomial models we set dispersion \\\xi\\ to 0.

We aggregate the data using the classification formed by the inner
variables. We identify any rows in the data where there are duplicated
combinations of the inner variables. For instance, if the inner
variables are `age` and `sex`, and we have two rows where `age` is
`"20-24"` and sex is `"Female"`, then these rows would count as
duplicated combinations. We aggregate offset and outcome variables
across these duplicates. Let \\D\\ be the number of times a particular
combination is duplicated.

With Poisson models, the aggregation formula for outcomes is
\\\begin{equation} y^{\text{new}} = \sum\_{i=1}^D y_i^{\text{old}},
\end{equation}\\ and the aggregation formula for exposure is
\\\begin{equation} w^{\text{new}} = \begin{cases} 1 & \text{if }
w_i^{\text{old}} \equiv 1 \\ \sum\_{i=1}^D w_i^{\text{old}} &
\text{otherwise}. \end{cases} \end{equation}\\

With binomial models, the aggregation formula for outcomes is
\\\begin{equation} y^{\text{new}} = \sum\_{i=1}^D y_i^{\text{old}},
\end{equation}\\ and the aggregation formula for size is
\\\begin{equation} w^{\text{new}} = \sum\_{i=1}^D w_i^{\text{old}}.
\end{equation}\\

With normal models, the aggregation formula for outcomes is
\\\begin{equation} y^{\text{new}} = \frac{\sum\_{i=1}^D
y_i^{\text{old}}}{D}, \end{equation}\\ and the aggregation formuala for
weights is \\\begin{equation} w^{\text{new}} = \frac{D^2}{\sum\_{i=1}^D
\frac{1}{w_i^{\text{old}}}}. \end{equation}\\

We remove all terms not involving ‘inner’ variables, other than the
intercept term, from the model. We fit the resulting model.

#### 10.2.3 Step 2: Fit outer model

Let \\\hat{\mu}\_i^{\text{in}}\\ be point estimates for the linear
predictor \\\mu_i\\ obtained from the inner model.

**Poisson model**

Aggregate the data using the classification formed by the outer
variables and any covariates. Remove all terms involving the ‘inner’
variables, plus the intercept, from the model. Set exposure to
\\w_i^{\text{out}} = \hat{\mu}^{\text{in}} w_i\\. Set dispersion to 0.
Fit the model.

**Binomial model**

Do not aggregate. Fit the original model, but with dispersion set to
zero, and using Known priors for ‘inner’ terms, based on point estimates
from the inner model.

**Normal model**

Aggregate the data using the classification formed by the outer
variables and any covariates. Remove all terms involving the ‘inner’
variables, plus the intercept, from the model. Set the outcome variable
to to \$y_i^{} = y_i - ^{}. Fit the model.

#### 10.2.4 Step 4: Concatenate estimates

Concatenate posterior distributions for the inner terms from the inner
model to posterior distributions for the outer terms from the outer
model.

#### 10.2.5 Step 5: Calculate dispersion

If the original model includes a dispersion term, then estimate
dispersion. Let \\\hat{\mu}\_i^{\text{comb}}\\ be point estimates for
the linear predictor obtained from the concatenated estimates.

**Poisson model**

Use the original disaggregated data, or, if the original data contains
more then 10,000 rows, select 10,000 rows at random from the original
data. Remove all terms from the original model except for the intercept.
Set exposure to \\w_i^{\text{out}} = \hat{\mu}^{\text{comb}} w_i\\.

**Binomial model**

Fit the the original model, but with all terms except the intercept
having Known priors, where the values are obtained from point estimates
from the concatenated estimates.

**Normal model**

Use the original disaggregated data, or, if the original data contains
more then 10,000 rows, select 10,000 rows at random from the original
data. Remove all terms from the original model except for the intercept.
Set the outcome to \\y_i^{\text{out}} = y_i - \hat{\mu}^{\text{comb}}\\.

## 11 Deriving outputs

Running TMB yields a set of means \\\pmb{m}\\, and a precision matrix
\\\pmb{Q}^{-1}\\, which together define the approximate joint posterior
distribution of

- intercept, main effects, and internactions \\\pmb{\beta}^{(m)}\\, \\m
  = 0, \cdots, M\\,
- typically, hyper-parameters for the \\\pmb{\beta}^{(m)}\\, in many
  cases transformed to another scale, such as a log scale,
- optionally, dispersion term \\\xi\\, and
- optionally, covariate cofficients vector \\\pmb{zeta}\\.

Let \\\tilde{\pmb{\theta}}\\ to denote a vector containing all these
quantities. We draw values for \\\tilde{\pmb{\theta}} \mid \pmb{m},
\pmb{Q}^{-1}\\, typically using sparse matrix methods (as implemented in
package **sparseMVN**.

Next we convert any transformed hyper-parameters back to the original
units, and insert values for \\\pmb{\beta}^{(m)}\\ for terms that have
Known priors. We denote the resulting vector \\\pmb{\theta}\\.

We draw from the distribution of \\\pmb{\gamma} \mid \pmb{y},
\pmb{\theta}\\ using the methods described in Sections
[3.1](#sec:pois)-[3.3](#sec:norm).

Output from the Normal model receives special treatment. As described in
section [3.3](#sec:norm) and [4](#sec:means), the Normal model is
\\\begin{align} \frac{y_i - \bar{y}}{s} & \sim \text{N}\left(\mu_i,
\frac{\bar{w}}{w_i} \xi^2\right) \\ \mu_i & = \sum\_{m=0}^{M}
\beta\_{j_i^m}^{(m)} + (\pmb{Z} \pmb{\zeta})\_i \end{align}\\

## 12 Simulation

To generate one set of simulated values, we start with values for
exposure, trials, or weights, \\\pmb{w}\\, and possibly covariates
\\\pmb{Z}\\, then go through the following steps:

1.  Draw values for any parameters in the priors for the
    \\\pmb{\beta}^{(m)}\\, \\m = 1, \cdots, M\\.
2.  Conditional on the values drawn in Step 1, draw values the
    \\\pmb{\beta}^{(m)}\\, \\m = 0, \cdots, M\\.
3.  If the model contains seasonal effects, draw the standard deviation
    \\\kappa_m\\, and then the effects \\\pmb{\lambda}^{(m)}\\.
4.  If the model contains covariates, draw \\\varphi\\ and
    \\\vartheta_p\\ where necessary, draw coefficient vector
    \\\pmb{\zeta}\\.
5.  Use values from steps 2–4 to form the linear predictor
    \\\sum\_{m=0}^{M} \pmb{X}^{(m)} (\pmb{\beta}^{(m)} +
    \pmb{\lambda}^{(m)}) + \pmb{Z} \pmb{\zeta}\\.
6.  Back-transform the linear predictor, to obtain vector of
    cell-specific parameters \\\pmb{\mu}\\.
7.  If the model contains a dispersion parameter \\\xi\\, draw values
    from the prior for \\\xi\\.
8.  In Poisson and binomial models, use \\\pmb{\mu}\\ and, if present,
    \\\xi\\ to draw \\\pmb{\gamma}\\.
9.  In Poisson and binomial models, use \\\pmb{\gamma}\\ and \\\pmb{w}\\
    to draw \\\pmb{y}\\; in normal models, use \\\pmb{\mu}\\, \\\xi\\,
    and \\\pmb{w}\\ to draw \\\pmb{y}\\.

## 13 Replicate data

### 13.1 Model

#### 13.1.1 Poisson likelihood

##### 13.1.1.1 Condition on \\\pmb{\gamma}\\

\\\begin{equation} y_i^{\text{rep}} \sim \text{Poisson}(\gamma_i w_i)
\end{equation}\\

##### 13.1.1.2 Condition on \\(\pmb{\mu}, \xi)\\

\\\begin{align} y_i^{\text{rep}} & \sim
\text{Poisson}(\gamma_i^{\text{rep}} w_i) \\ \gamma_i^{\text{rep}} &
\sim \text{Gamma}(\xi^{-1}, (\xi \mu_i)^{-1}) \end{align}\\ which is
equivalent to \\\begin{equation} y_i^{\text{rep}} \sim
\text{NegBinom}\left(\xi^{-1}, (1 + \mu_i w_i \xi)^{-1}\right)
\end{equation}\\

#### 13.1.2 Binomial likelihood

##### 13.1.2.1 Condition on \\\pmb{\gamma}\\

\\\begin{equation} y_i^{\text{rep}} \sim \text{Binomial}(w_i, \gamma_i)
\end{equation}\\

##### 13.1.2.2 Condition on \\(\pmb{\mu}, \xi)\\

\\\begin{align} y_i^{\text{rep}} & \sim \text{Binomial}(w_im
\gamma_i^{\text{rep}}) \\ \gamma_i^{\text{rep}} & \sim
\text{Beta}\left(\xi^{-1} \mu_i, \xi^{-1}(1 - \mu_i)\right)
\end{align}\\

#### 13.1.3 Normal likelihood

\\\begin{equation} y_i^{\text{rep}} \sim \text{N}(\gamma_i, \xi^2 / w_i)
\end{equation}\\

#### 13.1.4 Data models for outcomes

If the overall model includes a data model for the outcome, then a
further set of draws is made, deriving values for the observed outcomes,
given values for the true outcomes.

### 13.2 Code

    replicate_data(x, condition_on = c("fitted", "expected"), n = 20)

## 14 Appendices

### 14.1 Definitions

TODO - UPDATE THIS

| Quantity                        | Definition                                                                                           |
|:--------------------------------|:-----------------------------------------------------------------------------------------------------|
| \\i\\                           | Index for cell, \\i = 1, \cdots, n\\.                                                                |
| \\y_i\\                         | Value for outcome variable.                                                                          |
| \\w_i\\                         | Exposure, number of trials, or weight.                                                               |
| \\\gamma_i\\                    | Super-population rate, probability, or mean.                                                         |
| \\\mu_i\\                       | Cell-specific mean.                                                                                  |
| \\\xi\\                         | Dispersion parameter.                                                                                |
| \\g()\\                         | Log, logit, or identity function.                                                                    |
| \\m\\                           | Index for intercept, main effect, or interaction. \\m = 0, \cdots, M\\.                              |
| \\j\\                           | Index for element of a main effect or interaction.                                                   |
| \\u\\                           | Index for combination of ‘by’ variables for an interaction. \\u = 1, \cdots U_m\\. \\U_m V_m = J_m\\ |
| \\v\\                           | Index for the ‘along’ dimension of an interaction. \\v = 1, \cdots V_m\\. \\U_m V_m = J_m\\          |
| \\\beta^{(0)}\\                 | Intercept.                                                                                           |
| \\\pmb{\beta}^{(m)}\\           | Main effect or interaction. \\m = 1, \cdots, M\\.                                                    |
| \\\beta_j^{(m)}\\               | \\j\\th element of \\\pmb{\beta}^{(m)}\\. \\j = 1, \cdots, J_m\\.                                    |
| \\\pmb{X}^{(m)}\\               | Matrix mapping \\\pmb{\beta}^{(m)}\\ to \\\pmb{y}\\.                                                 |
| \\\pmb{Z}\\                     | Matrix of covariates.                                                                                |
| \\\pmb{\zeta}\\                 | Parameter vector for covariates \\\pmb{Z}^{(m)}\\.                                                   |
| \\A_0\\                         | Scale parameter in prior for intercept \\\beta^{(0)}\\ or initial value.                             |
| \\\tau_m\\                      | Standard deviation parameter for main effect or interaction.                                         |
| \\A\_{\tau}^{(m)}\\             | Scale parameter in prior for \\\tau_m\\.                                                             |
| \\\pmb{\alpha}^{(m)}\\          | Parameter vector for P-spline and SVD priors.                                                        |
| \\\alpha_k^{(m)}\\              | \\k\\th element of \\\pmb{\alpha}^{(m)}\\. \\k = 1, \cdots, K_m\\.                                   |
| \\\pmb{V}^{(m)}\\               | Covariance matrix for multivariate normal prior.                                                     |
| \\h_j^{(m)}\\                   | Linear covariate                                                                                     |
| \\\eta^{(m)}\\                  | Parameter specific to main effect or interaction \\\pmb{\beta}^{(m)}\\.                              |
| \\\eta_u^{(m)}\\                | Parameter specific to \\u\\th combination of ‘by’ variables in interaction \\\pmb{\beta}^{(m)}\\.    |
| \\A\_{\eta}^{(m)}\\             | Standard deviation in normal prior for \\\eta_m\\.                                                   |
| \\\omega_m\\                    | Standard deviation of parameter \\\eta_c\\ in multivariate priors.                                   |
| \\\phi_m\\                      | Correlation coefficient in AR1 densities.                                                            |
| \\a\_{0m}\\, \\a\_{1m}\\        | Minimum and maximum values for \\\phi_m\\.                                                           |
| \\\pmb{B}^{(m)}\\               | B-spline matrix in P-spline prior.                                                                   |
| \\\pmb{b}\_k^{(m)}\\            | B-spline. \\k = 1, \cdots, K_m\\.                                                                    |
| \\\pmb{F}^{(m)}\\               | Matrix in SVD prior.                                                                                 |
| \\\pmb{g}^{(m)}\\               | Offset in SVD prior.                                                                                 |
| \\\pmb{\beta}\_{\text{trend}}\\ | Trend effect.                                                                                        |
| \\\pmb{\beta}\_{\text{cyc}}\\   | Cyclical effect.                                                                                     |
| \\\pmb{\beta}\_{\text{seas}}\\  | Seasonal effect.                                                                                     |
| \\\varphi\\                     | Global shrinkage parameter in shrinkage prior.                                                       |
| \\A\_{\varphi}\\                | Scale term in prior for \\\varphi\\.                                                                 |
| \\\vartheta_p\\                 | Local shrinkage parameter in shrinkage prior.                                                        |
| \\p_0\\                         | Expected number of non-zero coefficients in \\\pmb{\zeta}\\.                                         |
| \\\hat{\sigma}\\                | Empirical scale estimate in prior for \\\varphi\\.                                                   |
| \\\pi\\                         | Vector of hyper-parameters                                                                           |

### 14.2 SVD prior for age

Let \\\pmb{A}\\ be a matrix of age-specific estimates from an
international database, transformed to take values in the range
\\(-\infty, \infty)\\. Each column of \\\pmb{A}\\ represents one set of
age-specific estimates, such as log mortality rates in Japan in 2010, or
logit labour participation rates in Germany in 1980.

Let \\\pmb{U}\\, \\\pmb{D}\\, \\\pmb{V}\\ be the matrices from a
singular value decomposition of \\\pmb{A}\\, where we have retained the
first \\K\\ components. Then \\\begin{equation} \pmb{A} \approx \pmb{U}
\pmb{D} \pmb{V}. \tag{14.1} \end{equation}\\

Let \\m_k\\ and \\s_k\\ be the mean and sample standard deviation of the
elements of the \\k\\th row of \\\pmb{V}\\, with \\\pmb{m} = (m_1,
\cdots, m_k)^{\top}\\ and \\\pmb{s} = (s_1, \cdots, s_k)^{\top}\\. Then
\\\begin{equation} \tilde{\pmb{V}} = (\text{diag}(\pmb{s}))^{-1}
(\pmb{V} - \pmb{m} \pmb{1}^{\top}) \end{equation}\\ is a standardized
version of \\\pmb{V}\\.

We can rewrite [(14.1)](#eq:svd1) as \\\begin{align} \pmb{A} & \approx
\pmb{U} \pmb{D} (\text{diag}(\pmb{s}) \tilde{\pmb{V}} + \pmb{m}
\pmb{1}^{\top}) \\ & = \pmb{F} \tilde{\pmb{V}} + \pmb{g} \pmb{1}^{\top},
\tag{14.2} \end{align}\\ where \\\pmb{F} = \pmb{U} \pmb{D}
\text{diag}(\pmb{s})\\ and \\\pmb{g} = \pmb{U} \pmb{D} \pmb{m}\\.

Let \\\tilde{\pmb{v}}\_l\\ be a randomly-selected column from
\\\tilde{\pmb{V}}\\. From the construction of \\\tilde{\pmb{V}}\\ we
have \\\text{E}\[\tilde{v}\_{kl}\] = 0\\ and
\\\text{var}\[\tilde{v}\_{kl}\] = 1\\. If \\\pmb{z}\\ is a vector of
standard normal variables, then \\\begin{equation} \pmb{F} \pmb{z} +
\pmb{g} \end{equation}\\ should look approximately like a
randomly-selected column from the original data matrix \\\pmb{A}\\.

### 14.3 Derivation of quantities used in data models

#### 14.3.1 Derivation of \\y_i^{\text{true}} \mid y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i, \xi, w_i\\

Derivation of formula from Section [8.2](#sec:datamod-pois-under)

When \\\xi \> 0\\, \\\begin{align} & p(y_i^{\text{true}} \mid
y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i, w_i, \xi) \\ & \propto
p(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]}, \mu_i, w_i,
\xi) p(y_i^{\text{true}} \mid \pi\_{g\[i\]}, \mu_i, w_i, \xi) \\ & =
\text{Binomial}(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]})
\times \text{NegBinom}(y_i^{\text{true}} \mid \xi^{-1}, \[1 + \mu_i w_i
\xi_i\]^{-1}) \\ & = \frac{y_i^{\text{true}}!}{y_i^{\text{obs}}!
(y_i^{\text{true}} - y_i^{\text{obs}})!}
\pi\_{g\[i\]}^{y_i^{\text{obs}}}
(1-\pi\_{g\[i\]})^{y_i^{\text{true}}-y_i^{\text{obs}}} \\ & \quad \times
\frac{\Gamma(y_i^{\text{true}} + \xi^{-1})}{y_i^{\text{true}}!
\Gamma(\xi^{-1})} \left( \frac{1}{ 1 + \mu_i w_i \xi} \right)^{\xi^{-1}}
\left( \frac{\mu_i w_i \xi}{1 + \mu_i w_i\\ \xi}
\right)^{y_i^{\text{true}}} \\ & \propto
\frac{\Gamma(y_i^{\text{true}} + \xi^{-1})}{(y_i^{\text{true}} -
y_i^{\text{obs}})!} \left( \frac{ (1-\pi\_{g\[i\]}) \mu_i w_i \xi}{1 +
\mu_i w_i \xi} \right)^{y_i^{\text{true}}}, \end{align}\\ which is the
kernel for the distribution \\\begin{equation} \text{NegBinom}\left(
y_i^{\text{true}} - y_i^{\text{obs}} \\\middle\|\\ y_i^{\text{obs}} +
\xi^{-1}, \frac{1 + \pi\_{h\[i\]} \mu_i w_i \xi}{1 + \mu_i w_i \xi}
\right). \end{equation}\\

When \\\xi \equiv 0\\, \\\begin{align} & p(y_i^{\text{true}} \mid
y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i, w_i) \\ & \propto
p(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]}, \mu_i, w_i)
p(y_i^{\text{true}} \mid \pi\_{g\[i\]}, \mu_i, w_i) \\ & =
\text{Binomial}(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]})
\times \text{Poisson}(y_i^{\text{true}} \mid \mu_i w_i) \\ & =
\frac{y_i^{\text{true}}!}{y_i^{\text{obs}}! (y_i^{\text{true}} -
y_i^{\text{obs}})!} \pi\_{g\[i\]}^{y_i^{\text{obs}}}
(1-\pi\_{g\[i\]})^{y_i^{\text{true}}-y_i^{\text{obs}}} \times
\frac{1}{y_i^{\text{true}}!} (\mu_i w_i)^{y_i^{\text{true}}} e^{-\mu_i
w_i} \\ & \propto \frac{1}{(y_i^{\text{true}} - y_i^{\text{obs}})!}
(\[1-\pi\_{g\[i\]}\] \mu_i w_i)^{y_i^{\text{true}}}, \end{align}\\ which
is the kernel for the distribution \\\begin{equation} \text{Poisson}(
y_i^{\text{true}} - y_i^{\text{obs}} \mid \[1 - \pi\_{g\[i\]}\] \mu_i
w_i). \end{equation}\\

#### 14.3.2 Derivation of \\y_i^{\text{true}} \mid y_i^{\text{obs}}, \kappa\_{h\[i\]}, \mu_i, \xi, w_i\\ in overcount-only model

Derivation of formula from Section [8.3](#sec:datamod-pois-over).

Conditional on \\\gamma_i, \kappa\_{h\[i\]}, w_i\\, the quantities
\\y_i^{\text{true}}\\ and \\v_i\\ are independent Poisson variates,
\\\begin{align} y_i^{\text{true}} & \sim \text{Poisson}(\gamma_i w_i) \\
v_i & \sim \text{Poisson}(\kappa\_{h\[i\]} \gamma_i w_i), \end{align}\\
with \\y_i^{\text{true}} + v_i = y_i^{\text{obs}}\\, implying that,
\\\begin{align} p(y_i^{\text{true}} \mid y_i^{\text{obs}}, \gamma_i,
\kappa\_{h\[i\]}, w_i) & = \text{Binomial}\left(y_i^{\text{true}}
\\\middle\|\\ y_i^{\text{obs}}, \frac{ \gamma_i w_i}{\gamma_i w_i +
\kappa\_{h\[i\]} \gamma_i w_i}\right) \\ & =
\text{Binomial}\left(y_i^{\text{true}} \\\middle\|\\ y_i^{\text{obs}},
\frac{1}{1 + \kappa\_{h\[i\]}} \right). \end{align}\\ The value of
\\y_i^{\text{true}}\\ does not depend on \\\gamma_i\\, and hence does
not depend on the determinants of \\\gamma_i\\, that is, on \\\mu_i\\
and \\\xi\\. We therefore have \\\begin{align} p(y_i^{\text{true}} \mid
y_i^{\text{obs}}, \mu_i, \xi, \kappa\_{h\[i\]}, w_i) & =
\text{Binomial}\left(y_i^{\text{true}} \\\middle\|\\ y_i^{\text{obs}},
\frac{1}{1 + \kappa\_{h\[i\]}} \right). \end{align}\\

#### 14.3.3 Derivation of \\u_i \mid y_i^{\text{obs}}, \pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, \xi, w_i\\

Derivation of formula from Section [8.4](#sec:datamod-pois-miscount).

Conditional on \\\gamma_i, \pi\_{g\[i\]}, \kappa\_{h\[i\]}, w_i\\, the
quantities \\u_i\\ and \\v_i\\ are independent Poisson variates,
\\\begin{align} u_i & \sim \text{Poisson}(\pi\_{g\[i\]} \gamma_i w_i) \\
v_i & \sim \text{Poisson}(\kappa\_{h\[i\]} \gamma_i w_i), \end{align}\\
with \\u_i + v_i = y_i^{\text{obs}}\\, implying that, \\\begin{align}
p(u_i \mid y_i^{\text{obs}}, \gamma_i, \pi\_{g\[i\]}, \kappa\_{h\[i\]},
w_i) & = \text{Binomial}\left(u_i \\\middle\|\\ y_i^{\text{obs}},
\frac{\pi\_{g\[i\]} \gamma_i w_i}{\pi\_{g\[i\]} \gamma_i w_i +
\kappa\_{h\[i\]} \gamma_i w_i}\right) \\ & = \text{Binomial}\left(u_i
\\\middle\|\\ y_i^{\text{obs}}, \frac{\pi\_{g\[i\]}}{\pi\_{g\[i\]} +
\kappa\_{h\[i\]}} \right). \end{align}\\ The value of \\u_i\\ does not
depend on \\\gamma_i\\, and hence does not depend on \\\mu_i\\ and
\\\xi\\, the determinants of \\\gamma_i\\. We therefore have
\\\begin{align} p(u_i \mid y_i^{\text{obs}}, \mu_i, \xi, \pi\_{g\[i\]},
\kappa\_{h\[i\]}, w_i) & = \text{Binomial}\left(u_i \\\middle\|\\
y_i^{\text{obs}}, \frac{\pi\_{g\[i\]}}{\pi\_{g\[i\]} + \kappa\_{h\[i\]}}
\right) \end{align}\\

#### 14.3.4 Derivation of \\y_i^{\text{true}} \mid u_i, \pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, w_i, \xi\\

Derivation of formula from Section [8.4](#sec:datamod-pois-miscount).

When \\\xi \> 0\\, \\\begin{align} & p(y_i^{\text{true}} \mid u_i,
\pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, w_i, \xi) \\ & \propto p(u_i
\mid y_i^{\text{true}}, \pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, w_i,
\xi) p(y_i^{\text{true}} \mid \pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i,
w_i, \xi) \\ & = \text{Binomial}(u_i \mid y_i^{\text{true}},
\pi\_{g\[i\]}) \times \text{NegBinom}(y_i^{\text{true}} \mid \xi^{-1},
\[1 + \mu_i w_i \xi_i\]^{-1}) \\ & = \frac{y_i^{\text{true}}!}{u_i!
(y_i^{\text{true}} - u_i)!} \pi\_{g\[i\]}^{u_i}
(1-\pi\_{g\[i\]})^{y_i^{\text{true}}-u_i} \\ & \quad \times
\frac{\Gamma(y_i^{\text{true}} + \xi^{-1})}{y_i^{\text{true}}!
\Gamma(\xi^{-1})} \left( \frac{1}{ 1 + \mu_i w_i \xi} \right)^{\xi^{-1}}
\left( \frac{\mu_i w_i \xi}{1 + \mu_i w_i\\ \xi}
\right)^{y_i^{\text{true}}} \\ & \propto
\frac{\Gamma(y_i^{\text{true}} + \xi^{-1})}{(y_i^{\text{true}} - u_i)!}
\left( \frac{ (1-\pi\_{g\[i\]}) \mu_i w_i \xi}{1 + \mu_i w_i \xi}
\right)^{y_i^{\text{true}}}, \end{align}\\ which is the kernel for the
distribution \\\begin{equation} \text{NegBinom}\left(
y_i^{\text{true}} - u_i \\\middle\|\\ u_i + \xi^{-1}, \frac{1 +
\pi\_{h\[i\]} \mu_i w_i \xi}{1 + \mu_i w_i \xi} \right) \end{equation}\\

When \\\xi \equiv 0\\, \\\begin{align} & p(y_i^{\text{true}} \mid u_i,
\pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, w_i) \\ & \propto p(u_i \mid
y_i^{\text{true}}, \pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, w_i)
p(y_i^{\text{true}} \mid \pi\_{g\[i\]}, \kappa\_{h\[i\]}, \mu_i, w_i) \\
& = \text{Binomial}(u_i \mid y_i^{\text{true}}, \pi\_{g\[i\]}) \times
\text{Poisson}(y_i^{\text{true}} \mid \mu_i w_i) \\ & =
\frac{y_i^{\text{true}}!}{u_i! (y_i^{\text{true}} - u_i)!}
\pi\_{g\[i\]}^{u_i} (1-\pi\_{g\[i\]})^{y_i^{\text{true}}-u_i} \times
\frac{1}{y_i^{\text{true}}!} (\mu_i w_i)^{y_i^{\text{true}}} e^{-\mu_i
w_i} \\ & \propto \frac{1}{(y_i^{\text{true}} - u_i)!}
(\[1-\pi\_{g\[i\]}\] \mu_i w_i)^{y_i^{\text{true}}}, \end{align}\\ which
is the kernel for the distribution \\\begin{equation} \text{Poisson}(
y_i^{\text{true}} - u_i \mid (1-\pi\_{h\[i\]}) \mu_i w_i)
\end{equation}\\

#### 14.3.5 Derivation of \\p(y_i^{\text{true}} \mid y_i^{\text{obs}}, \lambda, \mu_i, w_i)\\

Derivation of formula from Section [8.5](#sec:datamod-pois-noise).

\\\begin{align} p(y_i^{\text{true}} \mid y_i^{\text{obs}}, m\_{g\[i\]},
\mu_i, w_i) &\propto p(y_i^{\text{obs}} \mid y_i^{\text{true}},
m\_{g\[i\]}) \\ p(y_i^{\text{true}} \mid \mu_i, w_i) \\\[6pt\] &\propto
I\_{\lvert y_i^{\text{true}} - y_i^{\text{obs}} \rvert}(2 m\_{g\[i\]})
\\ \frac{(\mu_i w_i)^{y_i^{\text{true}}}}{y_i^{\text{true}}!}
\end{align}\\

#### 14.3.6 Derivation of \\p(w_i^{\text{true}} \mid w_i^{\text{obs}})\\ in Section [8.6](#sec:datamod-pois-off)

Assume have no prior information on \\w_i^{\text{true}}\\, so that
\\w_i^{\text{true}} \sim \text{Unif}(0, \infty)\\. Let \\A = 2 +
d\_{g\[i\]}^{-1}\\ and \$B = (1 + d\_{g\[i\]}^{-1}) \$.

\\\begin{align} p(w_i^{\text{true}} \mid w_i^{\text{obs}}) & \propto
p(w_i^{\text{obs}} \mid w_i^{\text{true}}) p(w_i^{\text{true}}) \\ &
\propto p(w_i^{\text{obs}} \mid w_i^{\text{true}}) \\ & =
\text{InvGamma}(A, B w_i^{\text{true}}) \\ & = \frac{(B
w_i^{\text{true}})^A}{\Gamma(A)} (w_i^{\text{obs}})^{-A-1} \exp
\left(-\frac{B w_i^{\text{true}}}{w_i^{\text{obs}}} \right) \\ & \propto
(w_i^{\text{true}})^A \exp \left(-\frac{B}{w_i^{\text{obs}}}
w_i^{\text{true}} \right) \\ & \propto \text{Gamma}(w_i^{\text{true}}
\mid A + 1, B \[w_i^{\text{obs}}\]^{-1} ) \\ & =
\text{Gamma}(w_i^{\text{true}} \mid 3 + d\_{g\[i\]}^{-1}, (1 +
d\_{g\[i\]}^{-1}) \[w_i^{\text{obs}}\]^{-1}) \end{align}\\ as required.

#### 14.3.7 Derivation of \\y_i^{\text{true}} \mid y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i, \xi, w_i\\

Derivation of formula from Section [8.7](#sec:datamod-binom).

When \\\xi \> 0\\, \\\begin{align} & p(y_i^{\text{true}} \mid
y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i, w_i, \xi) \\ & \propto
p(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]}) \times
p(y_i^{\text{true}} \mid \mu_i, \xi) \\ & =
\text{Binomial}(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]})
\times \text{BetaBinomial}(y_i^{\text{true}} \mid w_i, \xi^{-1}\mu_i,
\xi^{-1}\[1 - \mu_i\]) \\ & =
\binom{y_i^{\text{true}}}{y_i^{\text{obs}}}
\pi\_{g\[i\]}^{y_i^{\text{obs}}} (1-\pi\_{g\[i\]})^{y_i^{\text{true}} -
y_i^{\text{obs}}} \times \binom{w_i}{y_i^{\text{true}}}
\frac{B(y_i^{\text{true}}+\xi^{-1}\mu_i,
w_i-y_i^{\text{true}}+\xi^{-1}\[1-\mu_i\])}{B(\xi^{-1}\mu_i,
\xi^{-1}\[1-\mu_i\])} \\ & \propto
\frac{1}{(y_i^{\text{true}}-y_i^{\text{obs}})!
(w_i-y_i^{\text{true}})!}\\ (1-\pi\_{g\[i\]})^{y_i^{\text{true}}}
B(y_i^{\text{true}}+\xi^{-1}\mu_i,
w_i-y_i^{\text{true}}+\xi^{-1}\[1-\mu_i\]) \end{align}\\

When \\\xi \equiv 0\\, \\\begin{align} & p(y_i^{\text{true}} \mid
y_i^{\text{obs}}, \pi\_{g\[i\]}, \mu_i, w_i, \xi) \\ & \propto
p(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]}) \times
p(y_i^{\text{true}} \mid \mu_i, w_i) \\ & =
\text{Binomial}(y_i^{\text{obs}} \mid y_i^{\text{true}}, \pi\_{g\[i\]})
\times \text{Binomial}(y_i^{\text{true}} \mid w_i, \mu_i) \\ & =
\binom{y_i^{\text{true}}}{y_i^{\text{obs}}}
\pi\_{g\[i\]}^{y_i^{\text{obs}}} (1-\pi\_{g\[i\]})^{y_i^{\text{true}} -
y_i^{\text{obs}}} \times \binom{w_i}{y_i^{\text{true}}}
\mu_i^{y_i^{\text{true}}} (1 - \mu_i)^{w_i-y_i^{\text{true}}} \\ &
\propto \frac{1}{(y_i^{\text{true}}-y_i^{\text{obs}})!
(w_i-y_i^{\text{true}})!}\\
\left(\frac{\[1-\pi\_{g\[i\]}\]\mu_i}{1-\mu_i}\right)^{y_i^{\text{true}}},
\end{align}\\ which is the kernel for the distribution
\\\begin{equation} \text{Binomial}\left(y_i^{\text{true}} -
y_i^{\text{obs}} \\\middle\|\\ w_i - y_i^{\text{obs}}, \frac{\[1 -
\pi\_{g\[i\]}\] \mu_i}{1-\pi\_{g\[i\]} \mu_i}\right). \end{equation}\\

## References

Hyndman, Rob J, and Yeasmin Khandakar. 2008. “Automatic Time Series
Forecasting: The Forecast Package for R.” *Journal of Statistical
Software* 26 (3): 1–22. <https://doi.org/10.18637/jss.v027.i03>.

Norton, Richard A, J Andrés Christen, and Colin Fox. 2018. “Sampling
Hyperparameters in Hierarchical Models: Improving on Gibbs for
High-Dimensional Latent Fields and Large Datasets.” *Communications in
Statistics-Simulation and Computation* 47 (9): 2639–55.

Simpson, Dan. 2022. “Priors Part 4: Specifying Priors That Appropriately
Penalise Complexity.”
<https://dansblog.netlify.app/posts/2022-08-29-priors4/priors4.html>.

Wood, Simon N. 2017. *Generalized Additive Models: An Introduction with
R*. Chapman; Hall/CRC.
