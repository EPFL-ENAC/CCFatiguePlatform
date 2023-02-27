# Changelog

## [0.2.0](https://github.com/EPFL-ENAC/CCFatiguePlatform/compare/backend-v0.1.0...backend-v0.2.0) (2022-10-11)


### Features

* **api:** wip describe Test and TestMeasuringPointModel ([d8e525f](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/d8e525fc15a3ec768cb6689850a3e281bb0fa368))
* **DB, backend, API:** trickle up data convention ([33f21b1](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/33f21b1b069837252be7cdffb7dbb30c7c30027d)), closes [#70](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/70)
* **DB, backend, API:** trickle up data convention ([15ac3d1](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/15ac3d1218e6e323725a48f61760b64818c81329)), closes [#70](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/70)
* **db:** add test measuring_point ([ab18345](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/ab18345681634205de042d4a0fda944c4642b4db)), closes [#104](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/104)
* **echarts:** no bokeh tests dashboard [#65](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/65) ([a0a8e0d](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/a0a8e0da6d63fe533ff78b10d6169f8bb2bd7874))
* f2py compile windows ([606b744](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/606b744390623f0fcc26ce988f3eb15110c4c2ab))
* **frontend:** analysis echarts [#65](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/65) ([3d9cdde](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/3d9cdde34cdc4e64293fbd1da83eabc984d21c65))
* **frontend:** optimize r_ratio [#65](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/65) ([00dca1e](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/00dca1e1ae2276cdde79d805ebd28eefefea94ae))
* **hysteresis_loops.py:** wip process all csv ([78f5556](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/78f55561ea9d99d772f148499f51eb8dff539a0c))
* **make run:** ready to deploy on server ([55812fa](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/55812faa132eb25daaf7f2bf91bd03053124e8d0)), closes [#86](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/86)
* sn curve loglog bokeh ([cddc7a8](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/cddc7a8fa009d6deb9d90f3f487fce39904ed81a))
* sn curve sendeckyj ([e81057a](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/e81057af643aa42331b977b829d0f6ae10117961))
* **test data from CSV:** wip ([d5fe0da](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/d5fe0daad4562550f3eac3a59e6ef1777213ffd3))
* the return of sn curve ([cc07ccd](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/cc07ccd4f3eb8469a5d5489295f3851fb8167602))


### Bug Fixes

* **backend:** poetry dulwich issue ([88f0755](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/88f0755570626648e111c67061a3493b8ae790bc))
* **backend:** remove test measuring_points ([117b6e6](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/117b6e622d6f92aea8f45472d9047aadb4c59db6))
* **backend:** rename test_dashboard.py to experiment_test.py [#123](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/123) ([cc4d900](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/cc4d900bd675403f4351a7b382dbe2cd3c4dc5ec))
* csv format header for bokeh ([faa9ad5](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/faa9ad59a63ebc583d0cb0c84a1733a5b4e5bb50))
* **dotenv:** windows compatible .env ([ca0c4e9](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/ca0c4e97643706088ef698140138357677bccff8))
* **init_db:** inject all experiments ([a4012c1](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/a4012c1789a1308ca67601213633e158bd0009a0))
* **mod 3+5 harris:** merged common parts ([88808cb](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/88808cb626bfb287bd5501db33ec3f58038f1ef9))
* **mod 4 ftpf:** adapt to backend+testing ([28657f4](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/28657f41c505419cb05437f0816b3c567c13a159))
* **mod 4 ftpf:** moved to backend ([ccba57e](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/ccba57eb4fe482258d521f0d660171a46c5fa5b3))
* **mod cyc_range_mean:** adapt to backend+testing ([5904f3f](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/5904f3fff31e039de6b0d4bbf1414f9dc972cd29))
* **module 2:** adapt AGG changes to sendeckyj ([#115](https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/115)) ([c948197](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/c94819777ee56841bb7af6fca1d16713f8b6b278))
* remove fake data ([210af6f](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/210af6f073aa57c078e5a0def819f8a2e1544f46))
* STD -&gt; TST ([166514e](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/166514e1da712e3167050f55be8708132c3048ce))
* tests api ([5268ac5](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/5268ac56ccc9e37bc0905705d9a6804d9a4d74e3))
* **tests:** fix vscode config + create output folder ([edf6a19](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/edf6a198f1e77ff44124c89a84a52d726ee78cfd))


### Performance Improvements

* **hysteresis_loops.py:** use pandas ([92e8b10](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/92e8b109e4ad51deb1b4c118630548b4d118e956))


### Documentation

* add comments ([b97b78e](https://github.com/EPFL-ENAC/CCFatiguePlatform/commit/b97b78e78b0850f476300a5b546ef8ed354ec05f))
