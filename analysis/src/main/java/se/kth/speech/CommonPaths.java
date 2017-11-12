/**
 *
 */
package se.kth.speech;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Nov 2017
 *
 */
public final class CommonPaths {

	public static Path findCommonPrefixPath(final Collection<? extends Path> paths) {
		if (!paths.stream().allMatch(Path::isAbsolute)) {
			throw new IllegalArgumentException("All supplied paths must be absolute.");
		}
		final List<Iterator<Path>> pathIters = paths.stream().map(Path::iterator)
				.collect(Collectors.toCollection(() -> new ArrayList<>(paths.size())));
		final int maxPrefixLength = paths.stream().mapToInt(Path::getNameCount).min().orElse(0);
		final List<Path> prefixComponents = new ArrayList<>(maxPrefixLength);
		pathComponentIter: for (int pathComponentIter = 0; pathComponentIter < maxPrefixLength; ++pathComponentIter) {
			for (final Iterator<Iterator<Path>> pathIterIter = pathIters.iterator(); pathIterIter.hasNext();) {
				final Iterator<Path> firstPathIter = pathIterIter.next();
				final Path pathComponent = firstPathIter.next();
				while (pathIterIter.hasNext()) {
					final Iterator<Path> nextPathIter = pathIterIter.next();
					final Path nextPathComponent = nextPathIter.next();
					if (!pathComponent.equals(nextPathComponent)) {
						break pathComponentIter;
					}
				}
				prefixComponents.add(pathComponent);
			}
		}

		Path result = Paths.get("/");
		for (final Path prefixComponent : prefixComponents) {
			result = result.resolve(prefixComponent);
		}
		return result;
	}

	private CommonPaths() {
	}

}
