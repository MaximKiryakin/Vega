Проект выполнили: Кирякин Максим, Куренкова Дарья, Коваль Наталия

* Отчет находится в директории `Report/report.pdf`

* Код проекта находся в файлах `source/Project2.ipynb` и `source/Project2.R`

В работе проводилось сравнение двух классов моделей предсказания волатильности `HAR` и `GARCH`.
В качестве базового актива были выбраны акции Сбера. 

Всего рассматривалось два временных интервала:

1. с 3-ого января 2020 года по 29 ноября 2024 года
2. с 1 января 2017 по 25 марта 2020.

Причем второй интервал рассматривался только для HAR моделей и считался более волатильным, чем первый.

В сравнении участвовали следующие модели семейства GARCH: GARCH, EGARCH, NAGARCH, TGARCH, IGARCH. И следующие модели семейства HAR: HAR, HARQ, HARJ.
На первом наборе данных среди GARCH моделей лучше всего себя показал TGARCH(1, 1)-sstd, а из семейства HAR -- обычная HAR модель. Притом на высоковолатильных данных результаты HAR и HARQ сопоставимы. Результаты работы отобранных алгоритмов приведены в таблице $\ref{tab:4}$.

| Период                     | Модель           |   MAE              | MSE             | MAPE  |
|----------------------------|------------------|--------------------|-----------------|-------|
| c 03-01-2020 по 29-11-2024 | TGARCH(1,1)-sstd | $2.2 * 10^{-2}$    | $4.9 * 10^{-4}$ | 0.994 |
|                            | GARCH(1,1)-norm  | $2.2 * 10^{-2}$    | $4.9 * 10^{-4}$ | 0.995 |
|                            | HAR              | $3.0 * 10^{-3}$    | $1.8 * 10^{-5}$ | 0.198 |
|                            | HARQ             | $4.2 * 10^{-3}$    | $3.2 * 10^{-5}$ | 0.286 |
|                            | HARJ             | $3.7 * 10^{-3}$    | $2.6 * 10^{-5}$ | 0.253 |
|                            | Naive            | $3.3 * 10^{-3}$    | $2.7 * 10^{-5}$ | 0.221 |
| c 01-01-2017 по 25-03-2020 | HAR              | $8.8 * 10^{-3}$    | $1.8 * 10^{-4}$ | 0.300 |
|                            | HARQ             | $9.0 * 10^{-3}$    | $1.9 * 10^{-4}$ | 0.305 |
|                            | HARJ             | $9.9 * 10^{-3}$    | $2.1 * 10^{-4}$ | 0.324 |
|                            | Naive            | $1.4 * 10^{-2}$    | $3.3 * 10^{-4}$ | 0.868 |


В результате экспериментов было установлено, что прогноз HAR моделей является гораздо более точным по сравнению с GARCH. Однако качество модели GARCH может быть улучшено, если делать прогноз не на день, а, например, на 5 мин вперед.

Кроме того, в таблицу также добавлены значения метрик наивного прогноза реализованной волатильности, в качестве которого бралось среднее значение за последние 3 дня. Эксперименты показывают, что для относительно неволатильных данных наивный прогноз даже превзошел HAR модели по метрике MAPE. При этом на волатильных данных наивный прогноз проигрывает HAR моделям. На основании этих результатов можно говорить об эффективности HAR моделей на волатильных данных.
