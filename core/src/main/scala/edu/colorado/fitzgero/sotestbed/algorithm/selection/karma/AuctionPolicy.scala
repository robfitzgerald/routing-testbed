package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

sealed trait AuctionPolicy

object AuctionPolicy {

  /**
    * all bids are processed regardless of outcome
    */
  case object UniformRedistribution extends AuctionPolicy

  /**
    * only winners (agents assigned user-optimal routes) must
    * pay out in the auction.
    */
  case object WinnersPayAll extends AuctionPolicy

  implicit class AuctionExtensionMethods(a: AuctionPolicy) {

    def resolveAuction(
      bidsAndSelectedRoutes: List[(Bid, Int, Path)],
      bank: Map[String, Karma],
      maxKarma: Karma
    ): Either[Error, Map[String, Karma]] = {
      a match {
        case UniformRedistribution =>
          val bids = bidsAndSelectedRoutes.map { case (bid, _, _) => bid }
          AuctionOps.resolveBidsUniformly(bids, bank, maxKarma)
        case WinnersPayAll =>
          AuctionOps.resolveBidsWinnersPayAll(bidsAndSelectedRoutes, bank, maxKarma)
      }
    }

  }

}
