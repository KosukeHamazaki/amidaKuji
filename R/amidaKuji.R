#' Create the base of Amida Kuji.
#'
#' @description This function creates the base of Amida Kuji by determining where the horizontal lines are put.
#'
#'
#' @param nVerticalLines The number of vertical lines in Amida Kuji. It should be equal or larger than the number of players.
#' @param nHorizontalLines The number of horizontal lines in Amida Kuji.
#'  Of course, more this value increase, more the AMidakuji becomes complex.
#' @param nMinBetweenLines The minumum number of lines between each vertical lines.
#' If this value <= 1, there is a possibility to occur a bias in Amida Kuji.
#' @param lenVertical The length of vertical lines. This parameter should not influence the results.
#' @param goalIDs The IDs for goals. The length of this argument should be equal to `nVerticalLines`.
#' @param plotKuji If TRUE, plot Amida Kuji in this function.
#' @param noCross If TRUE, there will be no cross in AMida Kuji. The default is FALSE
#' @param playerNames The names of players. The length of this argument should be equal to `nVerticalLines`.
#'
#' @return
#' Nothing, but create information about Amida Kuji as global objects.
#'
amidaKujiCreate <- function (nVerticalLines = 12, nHorizontalLines = 30,
                             nMinBetweenLines = 2, lenVertical = 10,
                             goalIDs = paste0("G", 1:nVerticalLines), plotKuji = TRUE,
                             noCross = FALSE, playerNames = paste0("P", 1:nVerticalLines)) {
  nBetween <- nVerticalLines - 1
  nBetweenLinesMinNow <- 0
  while (nBetweenLinesMinNow < nMinBetweenLines){
    horizontalPos <- rep(list(NA), nHorizontalLines)
    for (i in 1:nHorizontalLines) {
      horizontalStartNow <- sample(1:nBetween, 1)
      if (noCross){
        horizontalPosNow <- rep(lenVertical - runif(n = 1, min = 1, max = lenVertical), 2)
      } else {
        horizontalPosNow <- lenVertical - runif(n = 2, min = 1, max = lenVertical)
      }

      horizontalPosRightNow <- c(horizontalStartNow, horizontalPosNow[1])
      horizontalPosLeftNow <- c(horizontalStartNow + 1, horizontalPosNow[2])

      horizontalPos[[i]] <- rbind(horizontalPosRightNow, horizontalPosLeftNow)
    }

    nBetweenLinesNow <- table(unlist(lapply(horizontalPos, function(x) x[1, 1])))
    if(length(nBetweenLinesNow) < nBetween) {
      nBetweenLinesMinNow <- 0
    } else {
      nBetweenLinesMinNow <- min(nBetweenLinesNow)
    }
  }

  nVerticalLines <<- nVerticalLines
  nHorizontalLines <<- nHorizontalLines
  nMinBetweenLines <<- nMinBetweenLines

  lenVertical <<- lenVertical
  goalIDs <<- goalIDs
  playerNames <<- playerNames

  horizontalPos <<- horizontalPos

  if(plotKuji) {
    amidaKujiPlot()
  }
}





#' Perform Amida Kuji for one player.
#'
#' @description This function performs the Amida Kuji for one player of interest.
#'
#'
#' @param playerNo No. of the player of interest. This argument should be integer.
#' @param returnHistory Return the history of Amida Kuji or not.
#'
#' @return
#' \describe{
#' \item{playerNoFinal}{Where the player is at the final stage. Integer.}
#' \item{goalID}{The ID of the goal which corresponds to `playerNoFinal`}
#' \item{playerHistory}{The history of the path of the player. This shows the path that the player has passed.}
#' }
#'
#' @examples
#' ### Create the base of Amida Kuji.
#' amidaKujiCreate(nVerticalLines = 12, nHorizontalLines = 30, noCross = TRUE)
#'
#' ### Show all the results of Amida Kuji
#' for(playerNo in 1:nVerticalLines){
#' cat(paste0("player: ", playerNames[playerNo], "\n"))
#' print(amidaKuji(playerNo))
#' cat("--------------------------\n")
#' }
#'
amidaKuji <- function(playerNo, returnHistory = FALSE){
  playerNoNow <- playerNo
  horizontalPosWhichNo <- which.max(lapply(horizontalPos, function(x) {
    x[x[, 1] %in% playerNoNow, 2]
  }))

  maxHorizontalPosCands <- lenVertical
  posPathHistory <- c(playerNoNow, lenVertical)
  while(maxHorizontalPosCands > 0) {
    if((horizontalPos[[horizontalPosWhichNo]])[1, 1] == playerNoNow){
      horizontalPosWhichNow <- horizontalPos[[horizontalPosWhichNo]]
    } else {
      horizontalPosWhichNow <- (horizontalPos[[horizontalPosWhichNo]])[2:1, ]
    }
    posPathHistory <- rbind(posPathHistory, horizontalPosWhichNow)
    nextPos <- horizontalPosWhichNow[horizontalPosWhichNow[, 1] != playerNoNow, ]

    playerNoNow <- as.integer(nextPos[1])
    nextHorizontalPos <- nextPos[2]


    horizontalPosCands <- lapply(horizontalPos, function(x) {
      nextHorizontalPosCand <- x[x[, 1] %in% playerNoNow, 2]
      return(ifelse(nextHorizontalPosCand < nextHorizontalPos, nextHorizontalPosCand, 0))
    })
    maxHorizontalPosCands <- max(unlist(horizontalPosCands))
    if(maxHorizontalPosCands > 0){
      horizontalPosWhichNo <- which.max(horizontalPosCands)
    }
  }

  posPathHistory <- rbind(posPathHistory, c(playerNoNow, 0))
  rownames(posPathHistory) <- 0:(nrow(posPathHistory) - 1)
  colnames(posPathHistory) <- c("x_axis", "y_axis")

  if (returnHistory) {
    return(list(playerNoFinal = playerNoNow, goalID = goalIDs[playerNoNow],
                playerHistory = posPathHistory))
  } else {
    return(list(playerNoFinal = playerNoNow, goalID = goalIDs[playerNoNow]))
  }
}




#' Plot the Amida Kuji result
#'
#' @description This function shows the result of the Amida Kuji for one player of interest.
#'
#'
#' @param playerNo No. of the player of interest. This argument should be integer.
#'  If NULL, Only the base of Amida Kuji will be shown.
#' @param col Color of the history of the path of the player of interest.
#'
#' @return
#' Plot of Amida Kuji with some information.
#'
#'
#' @examples
#' ### Create the base of Amida Kuji.
#' amidaKujiCreate(nVerticalLines = 12, nHorizontalLines = 30, noCross = TRUE)
#'
#' ### Plot the results of Amida Kuji
#' amidaKujiPlot(playerNo = 4, col = 2)
#'
amidaKujiPlot <- function(playerNo = NULL, col = 2){
  plot(x = 1:nVerticalLines, y = 1:nVerticalLines, ylim = c(-2, lenVertical + 2),
       type = "n", axes = FALSE, xlab = "", ylab = "")
  segments(x0 = 1:nVerticalLines, y0 = rep(0, nVerticalLines),
           x1 = 1:nVerticalLines, y1 = rep(lenVertical), lwd = 1.8)
  segments(x0 = unlist(lapply(horizontalPos, function(x) x[1, 1])),
           y0 = unlist(lapply(horizontalPos, function(x) x[1, 2])),
           x1 = unlist(lapply(horizontalPos, function(x) x[2, 1])),
           y1 = unlist(lapply(horizontalPos, function(x) x[2, 2])),
           lwd = 0.8, col = "grey50")

  text(x = 1:nVerticalLines, y = rep(-1, nVerticalLines),
       label = goalIDs)

  if (!is.null(playerNo)) {
    amidaKujiRes <- amidaKuji(playerNo, returnHistory = T)
    playerHistory <- amidaKujiRes$playerHistory
    for (stepNow in 1:(nrow(playerHistory) - 1)) {
      segments(x0 = playerHistory[stepNow, 1],
               y0 = playerHistory[stepNow, 2],
               x1 = playerHistory[stepNow + 1, 1],
               y1 = playerHistory[stepNow + 1, 2],
               lwd = 1.4, col = col)
    }

    text(x = playerNo, y = lenVertical + 1,
         label = playerNames[playerNo], col = col, font = 2)
    text(x = amidaKujiRes$playerNoFinal, y = -1,
         label = amidaKujiRes$goalID, col = col, font = 2)
  }
}
