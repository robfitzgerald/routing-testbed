package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.Monad
import cats.data.OptionT
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

object RoutingOps {

  type PathToMarginalFlows[F[_], V, E] = (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]]

  def defaultMarginalFlow[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E], path: Path): F[List[(EdgeId, Flow)]] =
    Monad[F].pure{
      path.map{pathSegment =>
        (pathSegment.edgeId, Flow(1.0))
      }
    }

  def defaultCombineFlows(flows: Iterable[Flow]): Flow = flows.foldLeft(Flow.Zero){_ + _}

  /**
    * given a path, determine the flow assignment contribution per edge
    * @param roadNetwork
    * @param path
    * @tparam F
    * @tparam V
    * @tparam E
    * @return
    */
  def marginalDecayedFlows[F[_]: Monad, V, E](decayAfterEstimatedTime: SimTime, decayRate: Flow)(roadNetwork: RoadNetwork[F, V, E],
                                                                                    path                    : Path): F[List[(EdgeId, Flow)]] = {
    for {
      linkTuples <- path.traverse{pathSegment => roadNetwork.edge(pathSegment.edgeId).map{edge => (pathSegment, edge)} }
    } yield {
      for {
        (pathSegment, edgeOption) <- linkTuples
        edge <- edgeOption
      } yield {
        ???
      }
    }
  }
}
