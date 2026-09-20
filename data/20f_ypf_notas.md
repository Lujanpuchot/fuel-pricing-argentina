# Notas metodológicas — extracción 20-F YPF S.A. (SEC EDGAR), FY2004–FY2024

Fuente: informes anuales 20-F de YPF S.A. (CIK 0000904851), descargados de EDGAR el 13-sep-2026.
Regla general: cada año fiscal T se extrajo del 20-F propio de T (presentado en T+1), salvo donde se indica.
Archivo de datos: `20f_ypf_extract.csv` (UTF-8, separador coma, decimal punto).

## 1. Precio realizado del crudo en el mercado doméstico

Tres regímenes de presentación:

| Años | Label exacto | Unidad | Ubicación |
|---|---|---|---|
| 2004–2008 | "Average sales price — Oil (U.S.$ per barrel)" | US$/bbl | Item 4, tabla de producción por cuenca del Upstream. Nota al pie: "The average sales price per barrel of oil represents the transfer price established by YPF, which reflects the Argentine market price." |
| 2009–2021 | "Average oil sales price" (columna Argentina) | Ps./boe | Tabla "average production costs and average sales price by geographic area" |
| 2022–2024 | "Average oil sales price" (columna Argentina) | US$/boe | Misma tabla; el 20-F FY2022 fue el primero presentado íntegramente en USD |

- **2009–2019**: la columna `crudo_realizado_usd_bbl` es un **implícito** = Ps/boe ÷ tipo de cambio promedio del año **informado en el propio 20-F** (sección Exchange Rates, fila "Average"): 3,75 (2009); 3,92 (2010); 4,15 (2011); 4,58 (2012); 5,54 (2013); 8,23 (2014); 9,39 (2015); 14,78 (2016); 16,76 (2017); 29,32 (2018); 49,23 (2019). Los Ps/boe originales están en la columna `notas` del CSV.
- **2020 y 2021**: se usó el valor en US$/boe que reporta retrospectivamente el 20-F FY2022 (39,6 y 53,7). Los 20-F propios reportan 2.798,38 y 5.111,3 Ps/boe respectivamente (consistentes: 5.111,3/95,1≈53,7).
- **Serie resultante (US$/bbl)**: 2004: 31,39 · 2005: 35,53 · 2006: 42,81 · 2007: 44,57 · 2008: 42,32 · 2009: 41,88* · 2010: 49,37* · 2011: 58,96* · 2012: 69,24* · 2013: 70,90* · 2014: 72,09* · 2015: 66,22* · 2016: 58,41* · 2017: 53,01* · 2018: 60,53* · 2019: 47,77* · 2020: 39,6 · 2021: 53,7 · 2022: 64,4 · 2023: 62,5 · 2024: 68,2. (* = implícito con TC del propio filing.)
- **Precios de mercado Medanito / Escalante (US$/bbl) citados en la narrativa de los 20-F** (no son el realizado de YPF):
  - 2015: Medanito 77 (ene) → 75 (nov); Escalante 63 → 61.
  - 2016: 67,50 / 54,90 (ene–jul); luego −2% mensual ago–oct (−6% acumulado).
  - 2017: Acuerdo de Transición: Medanito 59,40 (ene) → 55,00 (jul–dic); Escalante 48,30 → 47,00; suspendido el 13-sep-2017 (convergencia con paridad internacional).
  - 2020: promedio Medanito 41,3 / Escalante 40,8. **Barril Criollo**: Decreto 488/2020 (19-may-2020) fijó US$45/bbl como referencia Medanito para entregas locales; dejó de regir el 28-ago-2020 (Brent > 45 por 10 días consecutivos).
  - 2021: 53,7 / 58,9 · 2022: 62,3 / 70,9 (Brent 98,9) · 2023: 60,8 / 68,7 (Brent 82,2) · 2024: 68,9 / 72,7 (Brent 79,8).
- Restatements menores entre filings: el 20-F FY2008 muestra 2007 = 44,60 (propio: 44,57) y 2006 = 42,65 (propio: 42,81). Se usó siempre el filing propio.
- Anomalía 2012: la columna "Total" (288,71 Ps/boe) es menor que Argentina (317,11) y EE.UU. (466,75) simultáneamente; el dato se repite igual en los 20-F FY2012–FY2014. Se usó Argentina.

## 2. Costo de refinación

- **2004–2013**: los 20-F publican un nivel explícito "Refining cost per barrel" en **pesos por barril** dentro del MD&A del segmento Refining and Marketing / Downstream: 7,2 (2004); 7,6 (2005); 9,3 (2006); 10,7 (2007); 12,7 (2008); 14,8 (2009); 17,8 (2010); 22,9 (2011); 26,3 (2012); 37,5 (2013).
  - Definición (estable desde 2007): "the segment's cost of sales [2009+: production costs] for the period **less crude oil purchase costs** [2007: and depreciation], divided by the number of barrels produced during the period". Es un cash cost aproximado; 2007 excluye depreciación explícitamente, 2008+ no la menciona.
  - En USD implícito (TC promedio del filing): ≈2,4–3,0 US$/bbl en 2004–2006; 3,4 (2007); 4,0 (2008); 3,9 (2009); 4,5 (2010); 5,5 (2011); 5,7 (2012); 6,8 (2013) — dentro del rango esperable 3–8 US$/bbl.
- **2014–2021**: desaparece el nivel unitario; solo variaciones: 2014 +45% en ARS (y production costs +Ps 1.895 M, +51,5%); 2015 +Ps 912 M (+17,8%); 2016 +Ps 2.530 M (+42,0%, indicador unitario +44,2%); 2017 +Ps 1.762 M (+20,6%, unitario +21,1%); 2018 +Ps 2.812 M (+27,3%, unitario +31,5%); 2019 +Ps 11.580 M (+87,6%, unitario +91,8%); 2020 +Ps 4.505 M (+18,2%, sin unitario); 2021 +Ps 13.596 M (+54,8%, unitario +34,6%). Encadenando los % unitarios desde Ps 37,5 (2013) puede reconstruirse una serie aproximada (derivación propia, no publicada por YPF).
- **2022–2024**: concepto "refining and logistic(s) costs" en USD, solo variaciones: 2022 +US$343 M (+32,3%, unitario +25,1%) ⇒ nivel 2022 ≈ US$1.405 M; 2023 +US$189 M (+13,4%, unitario +10,0%) ⇒ nivel 2023 ≈ US$1.600 M (≈14,4 US$/bbl sobre 111,3 mmboe procesados — derivado, incluye logística); 2024 +US$189 M (sin %). El 20-F FY2022 también da 2021 vs 2020: +US$113 M (+11,9%, unitario −2,6%) ⇒ 2021 ≈ US$1.062 M, 2020 ≈ US$950 M.

## 3. Resultado operativo del Downstream

Cambios de perímetro y de moneda — **la serie NO es homogénea**:

| Años | Segmento | Moneda / marco | Valores (op. income) |
|---|---|---|---|
| 2004–2011 | "Refining and Marketing" | Ps millones, GAAP argentino | 1.324; 1.900; 258; 1.234; 3.089; 1.896; 3.313; 3.649 |
| 2012 | "Refining and marketing" | Ps millones, IFRS | 3.006 (Chemical: 913 aparte). El 20-F FY2013 re-expresa el nuevo segmento Downstream 2012 = 4.095 y 2011 = 5.466 |
| 2013–2015 | "Downstream" (= R&M + Química + distribución de gas + generación) | Ps millones, IFRS | 6.721; 10.978; 8.446 |
| 2016–2018 | "Downstream" (nueva segmentación: Gas & Power separado; 2015 re-expresado = 6.948) | Ps millones | 3.093; 15.813; 7.818 |
| 2019–2021 | "Downstream" | Ps millones (moneda funcional USD desde 2019, presentación en Ps) | 40.653; 4.839; 86.496 |
| 2022–2024 | "Downstream" — label "Operating profit / (loss)" | **US$ millones** | 1.523; 896; 1.306. El 20-F FY2022 re-expresa en USD: 2020 = 89 y 2021 = 945 |

## 4. Volumen de ventas de refinados

- **2004–2018: NA.** Los 20-F de esos años NO publican el volumen total de ventas de productos refinados de YPF (solo yields de producción de refinerías en mmbbl/miles de t, cifras país-total de la Secretaría de Energía, y variaciones % en el MD&A).
- **2019–2024**: tabla "YPF local market liquid fuels sales volumes" (naftas Super + Infinia y gasoil 500/800 ppm + Infinia 10 ppm, **solo mercado local**), en mcm/km3 (miles de m³): 12.610 (2019) y 10.013 (2020) según el 20-F FY2021; 12.063 (2021); 13.618 (2022, "excl. bunker y ventas a otras compañías"); 15.057 (2023); 14.132 (2024). Ojo: el 20-F FY2023 reporta 2022 = 14.561 y 2021 = 13.108 con una definición más amplia (sin la exclusión). **No es el total de refinados** (excluye fuel oil, jet, LPG, etc.); para margen por m³ usarlo solo como proxy de naftas+gasoil.

## 5. Crudo procesado

- Label: "Throughput crude/Feedstock" (2004–2011, solo el combinado) y "Throughput crude" + "Throughput crude (and) feedstock" por separado desde el 20-F FY2012. Unidad original: millones de barriles por año (mmbbl; desde FY2013 lo rotulan mmboe, numéricamente equivalente aquí). Refinerías 100% propias: La Plata, Luján de Cuyo, Plaza Huincul (no incluye el 50% de Refinor).
- Serie usada (2004–2011 = crudo+feedstock; 2012–2024 = solo crudo): 112,0; 113,1; 118,1; 122,0; 120,6; 114,0; 112,4; 107,0 | 105,4; 101,4; 106,0; 109,1; 107,4; 107,0; 103,6; 101,3; 85,8; 98,6; 104,2; 107,5; 110,0. Para 2011 el 20-F FY2012 desagrega: solo crudo = 103,8.
- Conversión a miles de m³/día (columna `crudo_procesado_km3d`): mmbbl × 158.987 m³/mmbbl ÷ 365 ÷ 1000 (no se ajustó por años bisiestos).
- Referencias narrativas: capacidad instalada ~319,5 mbbl/d en todo el período; "refinery output" incl. 50% Refinor: 334 mbbl/d (2007), 328 (2008).

## 6. Otras discontinuidades y advertencias

1. **FY2007** tiene dos enmiendas 20-F/A (abr-2008 y oct-2008); se usó el 20-F original (dp09492_20f.htm).
2. **Moneda**: Ps GAAP argentino (2004–2011) → Ps IFRS (2012–2018) → Ps con moneda funcional USD (2019–2021) → USD (2022–2024). Las series en pesos NO están ajustadas por inflación (YPF no aplicó IAS 29 por moneda funcional USD).
3. Los implícitos en USD usan el TC nominal promedio BCRA que informa cada 20-F; en años de cepo/brecha (2012–2015, 2019–2023) es el TC oficial.
4. El precio "realizado" del Upstream 2004–2013 incluye ventas intersegmento a precio de transferencia "que refleja el precio de mercado argentino" — es la variable pertinente para el netback doméstico, pero no es un precio arm's-length puro.
5. FY2020: op income Downstream Ps 4.839 M vs 89 MUSD en la re-presentación USD del 20-F FY2022 (4.839/70,6≈68,5; la diferencia es por conversión con moneda funcional USD, no un error de extracción).
6. Archivos fuente descargados y textos convertidos en `scratchpad/edgar/` (20f_AAAA.htm/.txt) por si hace falta re-verificar algún dato.
