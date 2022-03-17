# TaxBenefitGrossingUp

This Mathematica package allows for gross personal income estimation for individual on the basis of net personal income and relevant personal income tax (PIT) parameters such as tax allowances, PIT schedule, tax credit system, and social security contributions. This is implemented by GrossIncome[] function. The package also provides NetIncome[] function as a reverse to GrossIncome[]. Primary goal of the package is to enable data imputation of gross personal income in microdatabases used in macroeconomic budgetary and sustainability simulations where gross personal income computation is not trivial.

Both functions can be called with a similar structure of arguments and options:
- GrossIncome[*NetIncome*, *TaxAllowancesAmount*, *PersonalIncomeTaxSchedule*, *opts*] gives gross income estimated from a given net income, amount of tax allowances and tax schedule. 
- NetIncome[*GrossIncome*, *TaxAllowancesAmount*, *PersonalIncomeTaxSchedule*, *opts*] gives net income estimated from a given gross income, amount of tax allowances and tax schedule. 

*opts* define the arrangement of social security contributions and tax credit in a given computation.

Tax schedule should be provided as a list of tax brackets {{lo, up, marginal rate}, ...} where each tax bracket is defined as a list of its lower and upper margins and the tax rate, for instance:

`{{0, 20000, 0.15},
 {20000, 50000, 0.25},
 {50000, "-", 0.45}}`

Usually, the last bracket has no upper margin, this can be denoted by "-".

There are twelve possible combinations of *opts* depending on social security contribution rules and tax credits system. Social security contributions can be given by:
- schedule for social security contributions: 
`SocialSecuritySchedule->{{*lo*, *up*, *marginal rate*}, ...}`
- social security contributions as gross income proportion: 
`SocialSecurityProportion->*p*`
- social security contributions in absolute amount: 
`SocialSecurityAbsoluteAmount->`*`a`*`

Tax credit system can be given as:
- proportion of initial tax: 
`TaxCreditInitialPITProportion->*p*`
- proportion of gross income: 
`TaxCreditGrossIncomeProportion->*p*`
- in absolute amount: 
`TaxCreditAbsoluteAmount->*a*`
Tax credit can be ommited from the options if the taxation system 
does not allow for it.


### Examples

```
NetIncome[45000, 1200, { (* PIT schedule with three brackets *)
  {0, 20000, 0.15},
  {20000, 50000, 0.25},
  {50000, "-", 0.45}},
 SocialSecuritySchedule -> {(* 
   Social security schedule with three brackets *)
   {0, 10000, 0.17},
   {10000, 40000, 0.20},
   {40000, "-", 0.50}}] ===> 28400.
```

```
GrossIncome[28400, 1200, { (* PIT schedule with three brackets *)
  {0, 20000, 0.15},
  {20000, 50000, 0.25},
  {50000, "-", 0.45}},
 SocialSecuritySchedule -> {(* 
   Social security schedule with three brackets *)
   {0, 10000, 0.17},
   {10000, 40000, 0.20},
   {40000, "-", 0.50}}] ===> 45000.
```

```
NetIncome[10000, 1200, { (* PIT schedule with three brackets *)
  {0, 20000, 0.15},
  {20000, 50000, 0.25},
  {50000, "-", 0.45}},
 SocialSecurityProportion -> 0.05,
 TaxCreditInitialPITProportion -> 0.05] ===> 8317.25
```

```
GrossIncome[8317.25`, 1200, { (* PIT schedule with three brackets *)
  {0, 20000, 0.15},
  {20000, 50000, 0.25},
  {50000, "-", 0.45}},
 SocialSecurityProportion -> 0.05,
 TaxCreditInitialPITProportion -> 0.05] ===> 10000.
```

