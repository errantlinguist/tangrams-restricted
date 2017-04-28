/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package se.kth.speech.coin.tangrams.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator.Attribute;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javax.swing.JLabel;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialRegion;
import se.kth.speech.awt.CachingMaximumWidthFontFactory;
import se.kth.speech.awt.ComponentResizedEventListener;
import se.kth.speech.coin.tangrams.game.GameplayController;
import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class InteractiveGameViewFrame extends BasicGameViewFrame {

	private static final Logger LOGGER = LoggerFactory.getLogger(InteractiveGameViewFrame.class);

	private static final int MIN_ROLE_STATUS_LABEL_PADDING = 10;

	private static final Map<PlayerRole, String> PLAYER_ROLE_STATUS_LABEL_TEXT = createPlayerRoleStatusLabelTextMap();

	/**
	 *
	 */
	private static final long serialVersionUID = -4129777933223228599L;

	private static Map<PlayerRole, String> createPlayerRoleStatusLabelTextMap() {
		final Map<PlayerRole, String> result = new EnumMap<>(PlayerRole.class);
		result.put(PlayerRole.SELECTING, "Select piece to move.");
		result.put(PlayerRole.MOVE_SUBMISSION, "Continue to the next turn.");
		result.put(PlayerRole.SELECTION_CONFIRMATION, "Checking the other player's selection...");
		result.put(PlayerRole.WAITING_FOR_NEXT_MOVE, "Waiting for other player to submit next turn...");
		result.put(PlayerRole.WAITING_FOR_SELECTION, "Waiting for other player to select a piece to move...");
		result.put(PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION, "Waiting for other player to confirm selection...");
		assert result.size() == PlayerRole.values().length;
		return result;
	}

	private static Map<Attribute, Object> createRoleStatusFontAttrMap() {
		final Map<Attribute, Object> result = Maps.newHashMapWithExpectedSize(2);
		result.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		result.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
		// result.put(TextAttribute.SIZE, 32.0f);
		return result;
	}

	private static IntConsumer createRoleStatusLabelFontSizeUpdater(final Component comp,
			final float searchSizeIncrement, final Function<? super Font, FontMetrics> fmFactory) {
		final Font initialFont = comp.getFont();
		// Initialize the list to a size which is a function of the size of
		// the font as an estimate of the amount of fonts to search through
		final double maxSmallerFontCount = Math.ceil(comp.getFont().getSize2D());
		final double estBiggerFontCount = Math.ceil(maxSmallerFontCount / Math.log(maxSmallerFontCount));
		final int initialCapacity = (int) (maxSmallerFontCount + estBiggerFontCount);
		final List<Entry<Font, FontMetrics>> incrementingSizeFonts = new ArrayList<>(initialCapacity);
		final float startSize = 1.0f;
		{
			// Pre-populate expected possible re-sized fonts
			float size = startSize;
			while (incrementingSizeFonts.size() < initialCapacity) {
				final Font font = initialFont.deriveFont(size);
				final FontMetrics fm = fmFactory.apply(font);
				incrementingSizeFonts.add(new MutablePair<>(font, fm));
				size += searchSizeIncrement;
			}
		}
		return newWidth -> {
			final float endSize = Float.MAX_VALUE;
			final int padding = Math.max(newWidth / 24, MIN_ROLE_STATUS_LABEL_PADDING);
			final CachingMaximumWidthFontFactory fontFactory = new CachingMaximumWidthFontFactory(newWidth, fmFactory,
					initialFont, startSize, endSize, searchSizeIncrement, padding, incrementingSizeFonts::listIterator);
			final Font smallestRoleStatusLabelFont = PLAYER_ROLE_STATUS_LABEL_TEXT.values().stream().map(fontFactory)
					.collect(Collectors.minBy(Comparator.comparing(Font::getSize2D))).get();
			comp.setFont(smallestRoleStatusLabelFont);
		};
	}

	private final JLabel roleStatusLabel;

	private final IntConsumer roleStatusLabelFontSizeUpdater;

	InteractiveGameViewFrame(final AbstractGameBoardPanel boardPanel, final GameplayController controller,
			final Supplier<? extends MapEntryRemapping<Integer, SpatialRegion>> moveFactory,
			final Dimension preferredSize) {
		super(boardPanel, controller, moveFactory, preferredSize);
		final PlayerRole initialRole = controller.getRole();
		{
			final String labelText = PLAYER_ROLE_STATUS_LABEL_TEXT.get(initialRole);
			roleStatusLabel = new JLabel(labelText);
			final Font initialFont = roleStatusLabel.getFont().deriveFont(createRoleStatusFontAttrMap());
			roleStatusLabel.setFont(initialFont);
			roleStatusLabelFontSizeUpdater = createRoleStatusLabelFontSizeUpdater(roleStatusLabel, 1.0f,
					this::getFontMetrics);
			roleStatusLabelFontSizeUpdater.accept(preferredSize.width);
			addComponentListener(new ComponentResizedEventListener(this::updateRoleStatusLabelFontSize));
		}
		{
			final JPanel roleStatusPanel = new JPanel();
			roleStatusPanel.add(roleStatusLabel);
			add(roleStatusPanel, BorderLayout.PAGE_START);
		}

		addComponentListener(new ComponentAdapter() {

			/*
			 * (non-Javadoc)
			 *
			 * @see
			 * java.awt.event.ComponentAdapter#componentShown(java.awt.event
			 * .ComponentEvent)
			 */
			@Override
			public void componentShown(final ComponentEvent e) {
				updatePlayerRole(initialRole);
				removeComponentListener(this);
			}

		});
	}

	@Override
	public void updatePlayerRole(final PlayerRole newRole) {
		LOGGER.debug("Observed event representing a change in player role.");
		final String roleStatusText = PLAYER_ROLE_STATUS_LABEL_TEXT.get(newRole);
		LOGGER.info("Setting player state label for role {}.", newRole);
		roleStatusLabel.setText(roleStatusText);
		final String labelText = roleStatusLabel.getText();
		assert labelText != null && !labelText.isEmpty();
		super.updatePlayerRole(newRole);
	}

	private void updateRoleStatusLabelFontSize() {
		roleStatusLabelFontSizeUpdater.accept(getWidth());
	}

}
