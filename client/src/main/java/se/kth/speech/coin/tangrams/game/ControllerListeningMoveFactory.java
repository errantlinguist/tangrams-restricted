/**
 * 
 */
package se.kth.speech.coin.tangrams.game;

import java.util.function.Supplier;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.SpatialRegion;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 26 Dec 2017
 *
 */
public interface ControllerListeningMoveFactory extends Supplier<MapEntryRemapping<Integer, SpatialRegion>>, Controller.Listener {

}
