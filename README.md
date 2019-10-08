# amidaKuji
### Create Amida Kuji in R

#### Author: Kosuke Hamzaki (hamazaki@ut-biomet.org)
#### Date: 2019/10/08

このパッケージはあみだくじを作るためのRパッケージです。
縦の線の数（`nVerticalLines`引数）、横の線の数（`nHorizontalLines`引数）ともに自由に設定できます。
横の線同士が交わらないようにするか（`noCross`引数）なども設定できます。

注意点としては、はじめに必ず`amidaKujiCreate`関数であみだくじの設定を決め、その関数を実行してから、そのほかの関数（`amidaKuji`関数、`amidaKujiPlot`関数）を実行するようにしてください。
例としては以下の通りです。

``` r
amidaKujiCreate(nVerticalLines = 10 nHorizontalLines = 30, noCross = F)
for(playerNo in 1:nVerticalLines){
  cat(paste0("player: ", playerNames[playerNo], "\n"))
  print(amidaKuji(playerNo))
  cat("--------------------------\n")
}
amidaKujiPlot(playerNo = 4, col = 2)
```

後はそんなに難しくないので、ヘルプなどを見ながら実際に使っていただければと思います。
ご不明な点などあれば上記のメールアドレスまでご連絡ください。
