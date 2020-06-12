# Data

## Description

Estimated number of movements (n) in 3 days during the Covid crisis in some countries. 
You must select only country = IT.

The 3 days approximately refer to:

- the **first local lockdown** (2020-02-25 Lodi province in Lombardy region)

- the **global lockdown** in Italy (2020-03-10)

- the **end of the global lockdown** at regional level (05-05-2020)

Each dataset contains the following variables:

- `start_polygon_names` (starting province) and `end_polygon_names` (ending province) representing the nodes like in an edgelist.

- `start_name_stack` and `end_name_stack` are similar to the above variables except that they report the name of the region to which the province belongs.

- `length_km` reports the approximate length of the movement in km.

- `metric_name` reports the name of the various metrics to estimate movements:

- `n`: estimated raw number of movements
- `n_base`: the estimated pre-covid raw number of movements
- `n_diff`, `z_score` and `percent_change` are various function of the difference between pre-covid levels of movements and the current movements. Due to privacy policy when movements are too small they are capped to some extreme values.

- `metric_value` contains the values corresponding to one of the above mentioned metrics

The variables `start_x`, `start_y`, `end_x`, `end_y` report x-y coordinates of starting province and ending province

## Adjacency matrix

build as province-by-province for Italy from the edglist `start_polygon_names` to `end_polygon_names`.

## Weights

Select only one metric name and the corresponding value. E.g. `n`

## Node Attributes

Since you have to consider Italian provinces, their attributes can be downloaded from official sources.

We suggest: population size (of the whole province), region, area (north, centre, south), other variables (e.g., economic indicators).

## Example

`head(10)` of `2020-02-25.csv`:

```
utc_date	time	start_polygon_id	start_polygon_names	start_x	start_y	start_name_stack	end_polygon_id	end_polygon_names	end_x	end_y	end_name_stack	length_km	metric_name	metric_value	level	tile_size	country
25/02/2020	08:00	356078	Vas	16.74984501	47.15011862	Vas County	356079	Veszprém	17.64769231	47.12674808	Veszprém County	20.13375863	n_base	21.5	LEVEL2	12	HU
25/02/2020	08:00	356078	Vas	16.74984501	47.15011862	Vas County	356079	Veszprém	17.64769231	47.12674808	Veszprém County	20.13375863	n	13	LEVEL2	12	HU
25/02/2020	08:00	356078	Vas	16.74984501	47.15011862	Vas County	356079	Veszprém	17.64769231	47.12674808	Veszprém County	20.13375863	n_diff	-8.5	LEVEL2	12	HU
25/02/2020	08:00	356078	Vas	16.74984501	47.15011862	Vas County	356079	Veszprém	17.64769231	47.12674808	Veszprém County	20.13375863	percent_change	-37.77777778	LEVEL2	12	HU
25/02/2020	08:00	356078	Vas	16.74984501	47.15011862	Vas County	356079	Veszprém	17.64769231	47.12674808	Veszprém County	20.13375863	z_score	-1.296237848	LEVEL2	12	HU
25/02/2020	08:00	356077	Tolna	18.53801394	46.50757082	Tolna County	356062	Baranya	18.21059009	46.03611833	Baranya County	14.62234038	n_base	190.75	LEVEL2	12	HU
25/02/2020	08:00	356077	Tolna	18.53801394	46.50757082	Tolna County	356062	Baranya	18.21059009	46.03611833	Baranya County	14.62234038	n	213	LEVEL2	12	HU
25/02/2020	08:00	356077	Tolna	18.53801394	46.50757082	Tolna County	356062	Baranya	18.21059009	46.03611833	Baranya County	14.62234038	n_diff	22.25	LEVEL2	12	HU
25/02/2020	08:00	356077	Tolna	18.53801394	46.50757082	Tolna County	356062	Baranya	18.21059009	46.03611833	Baranya County	14.62234038	percent_change	11.60365059	LEVEL2	12	HU
25/02/2020	08:00	356077	Tolna	18.53801394	46.50757082	Tolna County	356062	Baranya	18.21059009	46.03611833	Baranya County	14.62234038	z_score	1.341928821	LEVEL2	12	HU
```