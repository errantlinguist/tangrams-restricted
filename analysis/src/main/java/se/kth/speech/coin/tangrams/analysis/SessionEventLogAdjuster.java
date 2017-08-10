/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ForkJoinPool;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Table;

import iristk.system.Event;
import se.kth.speech.TimestampArithmetic;
import se.kth.speech.awt.LookAndFeels;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.coin.tangrams.view.UserPrompts;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Aug 2017
 *
 */
final class SessionEventLogAdjuster {

	private enum AttributeType {
		EVENT(EventDialogue.class) {
			@Override
			protected int getValueListIdx(final int columnIndex) {
				return columnIndex;
			}

			@Override
			protected int getValueListSize(final TableColumnModel colModel) {
				return EVENT_ATTR_COUNT;
			}
		},
		UTTERANCE(Utterance.class) {
			@Override
			protected int getValueListIdx(final int columnIndex) {
				return columnIndex - EVENT_ATTR_COUNT;
			}

			@Override
			protected int getValueListSize(final TableColumnModel colModel) {
				return colModel.getColumnCount() - EVENT.getValueListSize(colModel);
			}
		};

		private static final int EVENT_ATTR_COUNT = EventAttribute.values().length;

		private final Class<?> valueClass;

		private AttributeType(final Class<?> valueClass) {
			this.valueClass = valueClass;
		}

		private List<TableColumn> createColumnList(final TableModel model, final TableColumnModel colModel) {
			final int resultSize = getValueListSize(colModel);
			final List<TableColumn> result = new ArrayList<>(resultSize);
			final Enumeration<TableColumn> cols = colModel.getColumns();

			int colIdx = 0;
			while (result.size() < resultSize && cols.hasMoreElements()) {
				final TableColumn col = cols.nextElement();
				if (isMatchingTypeColumn(model, colIdx)) {
					result.add(col);
				}
				colIdx++;
			}
			return result;
		}

		protected IntStream getMatchingTypeColumnIndices(final TableModel model) {
			return IntStream.range(0, model.getColumnCount()).filter(colIdx -> isMatchingTypeColumn(model, colIdx));
		}

		protected abstract int getValueListIdx(final int columnIndex);

		protected abstract int getValueListSize(final TableColumnModel colModel);

		protected boolean isMatchingTypeColumn(final TableModel model, final int colIdx) {
			final Class<?> colClass = model.getColumnClass(colIdx);
			return valueClass.isAssignableFrom(colClass);
		}
	}

	private enum EventAttribute {
		SENDER, TIME;
	}

	private static class EventDialogueAdjusterFrame extends JFrame {

		/**
		 *
		 */
		private static final long serialVersionUID = -5412327154602984470L;

		public EventDialogueAdjusterFrame(final String title, final LocalDateTime gameStartTime,
				final EventDialogueAdjusterTable diagTable) throws HeadlessException {
			super(title);
			final Container content = getContentPane();
			content.setLayout(new BoxLayout(content, BoxLayout.PAGE_AXIS));
			// https://stackoverflow.com/a/2452758/1391325
			content.add(new JScrollPane(diagTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED));

			final JPanel actionPanel = new JPanel();
			content.add(actionPanel);
			final JButton minimizeEventTimeButton = new JButton("Minimize event time");
			minimizeEventTimeButton.addActionListener(new FollowingEventTimeMinimizer(diagTable, this, gameStartTime));
			actionPanel.add(minimizeEventTimeButton);
		}

	}

	private static class EventDialogueAdjusterTable extends JTable {

		/**
		 *
		 */
		private static final long serialVersionUID = 5176564593731003375L;

		public EventDialogueAdjusterTable(final TableModel dm, final UtteranceCellRenderer uttCellRenderer) {
			super(dm);
			setCellSelectionEnabled(true);
			setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

			setColumnEventAttributeRenderers(getColumnModel());
			setDefaultRenderer(Utterance.class, uttCellRenderer);

			// Set widths using the newly-set renderers
			IntStream.range(0, getColumnCount()).forEach(this::setOptimumPreferredWidth);
		}

		private void setOptimumPreferredWidth(final int columnIdx) {
			final JTableHeader header = getTableHeader();
			final TableColumnModel colModel = getColumnModel();
			final TableColumn tableColumn = colModel.getColumn(columnIdx);
			final TableCellRenderer colRenderer = getColHeaderRenderer(tableColumn, header);
			final Object colHeaderValue = tableColumn.getHeaderValue();
			LOGGER.debug("Calculating width for column header value \"{}\", for column {}.", colHeaderValue, columnIdx);
			final Component colHeaderCellRendererComponent = colRenderer.getTableCellRendererComponent(this,
					colHeaderValue, false, false, -1, columnIdx);
			final int headerCellWdith = colHeaderCellRendererComponent.getPreferredSize().width;

			// https://tips4java.wordpress.com/2008/11/10/table-column-adjuster/
			final int intercellSpacingWidth = getIntercellSpacing().width;
			final int maxWidth = tableColumn.getMaxWidth();
			int preferredWidth = Math.max(headerCellWdith, tableColumn.getMinWidth());
			if (preferredWidth < maxWidth) {
				for (int row = 0; row < getRowCount(); row++) {
					final TableCellRenderer cellRenderer = getCellRenderer(row, columnIdx);
					final Component c = prepareRenderer(cellRenderer, row, columnIdx);
					final int cellWidth = c.getPreferredSize().width + intercellSpacingWidth;
					preferredWidth = Math.max(preferredWidth, cellWidth);

					if (preferredWidth >= maxWidth) {
						// We've exceeded the maximum width, no need to check
						// other rows
						preferredWidth = maxWidth;
						break;
					}
				}
			} else {
				// We've exceeded the maximum width, no need to the rows
				preferredWidth = maxWidth;
			}
			tableColumn.setPreferredWidth(preferredWidth);
		}

	}

	private static class EventDialogueFirstEventSenderRenderer extends DefaultTableCellRenderer {
		/**
		 *
		 */
		private static final long serialVersionUID = 1591902695449966670L;

		@Override
		public void setValue(final Object value) {
			// http://docs.oracle.com/javase/tutorial/uiswing/components/table.html#renderer
			final String repr;
			if (value == null) {
				repr = NULL_VALUE_REPR;
			} else {
				final EventDialogue eventDiag = (EventDialogue) value;
				final Optional<Event> firstEvent = eventDiag.getFirstEvent();
				repr = firstEvent.map(
						event -> event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString(), NULL_VALUE_REPR))
						.orElse(NULL_VALUE_REPR);
			}
			setText(repr);
		}
	}

	private static class EventDialogueFirstEventTimeRenderer extends DefaultTableCellRenderer {
		/**
		 *
		 */
		private static final long serialVersionUID = 1591902695449966670L;

		@Override
		public void setValue(final Object value) {
			// http://docs.oracle.com/javase/tutorial/uiswing/components/table.html#renderer
			final String repr;
			if (value == null) {
				repr = NULL_VALUE_REPR;
			} else {
				final EventDialogue eventDiag = (EventDialogue) value;
				final Optional<Event> firstEvent = eventDiag.getFirstEvent();
				repr = firstEvent.map(Event::getTime).orElse(NULL_VALUE_REPR);
			}
			setText(repr);
		}
	}

	private static class EventDialogueTableModel extends AbstractTableModel {

		/**
		 *
		 */
		private static final long serialVersionUID = -4475137472007680337L;

		private final EventDialogue[] diags;

		private EventDialogueTableModel(final EventDialogue[] diags) {
			this.diags = diags;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
		 */
		@Override
		public Class<?> getColumnClass(final int columnIndex) {
			final Class<?> result;
			final EventAttribute colEventAttr = getColumnAttribute(columnIndex);
			if (colEventAttr == null) {
				result = Utterance.class;
			} else {
				switch (colEventAttr) {
				case SENDER: {
					result = EventDialogue.class;
					break;
				}
				case TIME: {
					result = EventDialogue.class;
					break;
				}
				default: {
					throw new AssertionError("No logic for handing switch case.");
				}
				}
			}
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see javax.swing.table.TableModel#getColumnCount()
		 */
		@Override
		public int getColumnCount() {
			final int maxUttCount = Arrays.stream(diags).map(EventDialogue::getUtts).mapToInt(List::size).max()
					.orElse(0);
			return maxUttCount + 1;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
		 */
		@Override
		public String getColumnName(final int column) {
			final String result;
			final EventAttribute colEventAttr = getColumnAttribute(column);
			if (colEventAttr == null) {
				final int uttIdx = AttributeType.UTTERANCE.getValueListIdx(column);
				result = "UTT_" + uttIdx;
			} else {
				switch (colEventAttr) {
				case SENDER: {
					result = "FIRST_EVT_SENDER";
					break;
				}
				case TIME: {
					result = "FIRST_EVT_TIME";
					break;
				}
				default: {
					throw new AssertionError("No logic for handing switch case.");
				}
				}
			}
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see javax.swing.table.TableModel#getRowCount()
		 */
		@Override
		public int getRowCount() {
			return diags.length;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see javax.swing.table.TableModel#getValueAt(int, int)
		 */
		@Override
		public Object getValueAt(final int rowIndex, final int columnIndex) {
			final EventDialogue diag = diags[rowIndex];
			final Object result;
			final EventAttribute colEventAttr = getColumnAttribute(columnIndex);
			if (colEventAttr == null) {
				final List<Utterance> diagUtts = diag.getUtts();
				final int diagUttIdx = AttributeType.UTTERANCE.getValueListIdx(columnIndex);
				result = diagUtts.size() <= diagUttIdx ? null : diagUtts.get(diagUttIdx);
			} else {
				switch (colEventAttr) {
				case SENDER: {
					result = diag;
					break;
				}
				case TIME: {
					result = diag;
					break;
				}
				default: {
					throw new AssertionError("No logic for handing switch case.");
				}
				}
			}
			return result;
		}

	}

	private static class FollowingEventTimeMinimizer implements ActionListener {

		private final JTable diagTable;

		private final Component dialogueMessageParentComponent;

		private final LocalDateTime gameStartTime;

		private FollowingEventTimeMinimizer(final JTable diagTable, final Component dialogueMessageParentComponent,
				final LocalDateTime gameStartTime) {
			this.diagTable = diagTable;
			this.dialogueMessageParentComponent = dialogueMessageParentComponent;
			this.gameStartTime = gameStartTime;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(final ActionEvent event) {
			final TableModel model = diagTable.getModel();
			// Find the index of any column with Event instances as cell
			// values
			final int eventAttrColIdx = IntStream.range(0, diagTable.getColumnCount())
					.filter(colIdx -> AttributeType.EVENT.isMatchingTypeColumn(model, colIdx)).findAny().getAsInt();
			final int[] selectedRows = diagTable.getSelectedRows();
			for (final int rowIdx : selectedRows) {
				final IntStream uttColIdxs = AttributeType.UTTERANCE.getMatchingTypeColumnIndices(model);
				final Stream<Object> uttColVals = uttColIdxs.mapToObj(colIdx -> diagTable.getValueAt(rowIdx, colIdx));
				final Stream<Utterance> nonNullUtts = uttColVals.filter(Objects::nonNull).map(Utterance.class::cast);
				final Optional<Utterance> optLastUtt = nonNullUtts.reduce((first, second) -> second);
				if (optLastUtt.isPresent()) {
					final Utterance lastUtt = optLastUtt.get();
					final LocalDateTime lastUttEndTime = TimestampArithmetic.createOffsetTimestamp(gameStartTime,
							lastUtt.getEndTime());
					try {
						final EventDialogue followingRowEventDiag = (EventDialogue) diagTable.getValueAt(rowIdx + 1,
								eventAttrColIdx);
						final Optional<Event> optFollowingRowFirstEvent = followingRowEventDiag.getFirstEvent();
						if (optFollowingRowFirstEvent.isPresent()) {
							final Event followingRowFirstEvent = optFollowingRowFirstEvent.get();
							final String followingRowEventTimeStr = followingRowFirstEvent.getTime();
							final LocalDateTime followingRowEventTime = EventTimes
									.parseEventTime(followingRowEventTimeStr);
							final int timeCmp = lastUttEndTime.compareTo(followingRowEventTime);
							if (timeCmp < 0) {
								final String newEventTimeStr = EventTimes.FORMATTER.format(lastUttEndTime);
								followingRowFirstEvent.setTime(newEventTimeStr);
								LOGGER.info("Set time of event ID \"{}\" to \"{}\".", followingRowFirstEvent.getId(),
										newEventTimeStr);
								diagTable.repaint();
							} else if (timeCmp > 0) {
								JOptionPane.showMessageDialog(dialogueMessageParentComponent, String.format(
										"End time of preceding event's last utterance (\"%s\") is after the time of the following event (\"%s\", ID \"%s\"), i.e. the utterance overlaps the start of the next event.",
										EventTimes.FORMATTER.format(lastUttEndTime), followingRowEventTimeStr,
										followingRowFirstEvent.getId()), "Overlapping times",
										JOptionPane.WARNING_MESSAGE);
							} else {
								JOptionPane.showMessageDialog(dialogueMessageParentComponent, String.format(
										"Time of event ID \"%s\" is already equal to the end time of the preceding event's last utterance (\"%s\").",
										followingRowFirstEvent.getId(), followingRowEventTimeStr));
							}
						} else {
							JOptionPane.showMessageDialog(dialogueMessageParentComponent,
									"Following event dialogue does not have an event.", "No following event",
									JOptionPane.WARNING_MESSAGE);
						}

					} catch (final ArrayIndexOutOfBoundsException ex) {
						JOptionPane.showMessageDialog(dialogueMessageParentComponent,
								String.format("No row for index %s.", ex.getLocalizedMessage()),
								ex.getClass().getSimpleName(), JOptionPane.WARNING_MESSAGE);
					}

				} else {
					JOptionPane.showMessageDialog(dialogueMessageParentComponent,
							"Cannot minimize event time to last utterance end time because utterance list is empty.",
							"No utterances", JOptionPane.WARNING_MESSAGE);
				}
			}
		}

	}

	private static class Settings {

		private final Properties props;

		private Settings(final Properties props) {
			this.props = props;
		}

		public Optional<String> getInpath() {
			final String propVal = props.getProperty("inpath");
			return propVal == null ? Optional.empty() : Optional.of(propVal);
		}

		public void setInpath(final String propVal) {
			props.setProperty("inpath", propVal);
		}

		private Properties getProperties() {
			return props;
		}
	}

	private static class UtteranceCellRenderer extends DefaultTableCellRenderer {

		/**
		 *
		 */
		private static final long serialVersionUID = -259987246089416410L;

		private final Function<? super List<String>, String> tokenSeqReprFactory;

		private UtteranceCellRenderer(final Function<? super List<String>, String> tokenSeqReprFactory) {
			this.tokenSeqReprFactory = tokenSeqReprFactory;
		}

		@Override
		public void setValue(final Object value) {
			// http://docs.oracle.com/javase/tutorial/uiswing/components/table.html#renderer
			final String repr;
			if (value == null) {
				repr = "";
			} else {
				final Utterance utt = (Utterance) value;
				final List<String> uttTokens = utt.getTokens();
				final String tokenSeqRepr = tokenSeqReprFactory.apply(uttTokens);
				repr = String.format("**%s:** %s", utt.getSpeakerId(), tokenSeqRepr);
			}
			setText(repr);
		}
	}

	private static final Path CLASS_SETTINGS_INFILE_PATH;

	private static final FileNameExtensionFilter DEFAULT_FILE_FILTER;

	private static final int ESTIMATED_DIAGS_PER_SESSION = 96;

	private static final int ESTIMATED_UNIQUE_TOKEN_SEQ_PER_DIAG_COUNT = 8;

	private static final Map<EventAttribute, Integer> EVENT_ATTR_IDXS = createEventAttributeIndexMap();

	private static final EventDialogueFactory EVENT_DIAG_FACTORY = new EventDialogueFactory(
			new EventTypeMatcher(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Predicate<Event> EVENT_FILTER = LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER;

	private static final List<FileNameExtensionFilter> FILE_FILTERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventLogAdjuster.class);

	private static final String NULL_VALUE_REPR = "-";

	private static final Path SETTINGS_DIR;

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	static {
		SETTINGS_DIR = Paths.get(".settings");
		CLASS_SETTINGS_INFILE_PATH = SETTINGS_DIR.resolve(SessionEventLogAdjuster.class.getName() + ".properties");
	}

	static {
		FILE_FILTERS = Arrays.asList(new FileNameExtensionFilter("Property files (*.properties)", "properties"));
		DEFAULT_FILE_FILTER = FILE_FILTERS.iterator().next();
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		LookAndFeels.setLookAndFeel();
		if (args.length < 1) {
			final Optional<Path> inputInfile = promptInfile();
			if (inputInfile.isPresent()) {
				accept(inputInfile.get());
			}
		} else {
			final Path infile = Paths.get(args[0]);
			accept(infile);
		}
	}

	private static void accept(final Path infile) throws JAXBException, IOException {
		LOGGER.info("Reading \"{}\".", infile);
		final SessionDataManager sessionData = SessionDataManager.create(infile);
		final SessionEventLogAdjuster adjuster = new SessionEventLogAdjuster();
		adjuster.accept(sessionData);
	}

	private static Map<EventAttribute, Integer> createEventAttributeIndexMap() {
		final Map<EventAttribute, Integer> result = new EnumMap<>(EventAttribute.class);
		final EventAttribute[] attrs = EventAttribute.values();
		for (int i = 0; i < attrs.length; ++i) {
			final EventAttribute attr = attrs[i];
			result.put(attr, i);
		}
		assert result.size() == attrs.length;
		return result;
	}

	private static String createHistoryTitleStr(final String gameId, final GameHistory history) {
		return String.format("Game %s, started at %s", gameId, history.getStartTime());
	}

	private static TableCellRenderer getColHeaderRenderer(final TableColumn tableColumn, final JTableHeader header) {
		final TableCellRenderer colRenderer = tableColumn.getHeaderRenderer();
		return colRenderer == null ? header.getDefaultRenderer() : colRenderer;
	}

	private static EventAttribute getColumnAttribute(final int colIdx) {
		final EventAttribute[] atts = EventAttribute.values();
		return atts.length <= colIdx ? null : atts[colIdx];
	}

	private static TableColumn getEventAttributeColumn(final TableColumnModel colModel, final EventAttribute attr) {
		final int colIdx = EVENT_ATTR_IDXS.get(attr);
		return colModel.getColumn(colIdx);
	}

	private static Settings loadClassSettings() {
		final Properties settingsProps = new Properties();
		try {
			loadClassSettingsProps(settingsProps);
		} catch (final IOException e) {
			LOGGER.info(
					"A(n) {} occurred while trying to load the class settings from \"{}\"; Falling back to defaults.",
					e.getClass().getSimpleName(), CLASS_SETTINGS_INFILE_PATH);
		}
		return new Settings(settingsProps);
	}

	private static void loadClassSettingsProps(final Properties props) throws IOException {
		final Path classSettingsInfilePath = SETTINGS_DIR
				.resolve(SessionEventLogAdjuster.class.getName() + ".properties");
		Files.createDirectories(SETTINGS_DIR);
		try (InputStream classSettingsPropsInstream = Files.newInputStream(classSettingsInfilePath)) {
			props.load(classSettingsPropsInstream);
		}
	}

	private static Optional<Path> promptInfile() throws IOException {
		final Settings settings = loadClassSettings();
		final File currentInpath = new File(settings.getInpath().orElse(System.getProperty("user.dir")));
		final JFileChooser fileChooser = new JFileChooser(currentInpath);
		FILE_FILTERS.stream().forEachOrdered(fileChooser::addChoosableFileFilter);
		fileChooser.setFileFilter(DEFAULT_FILE_FILTER);
		fileChooser.setDialogTitle("Input file");
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		final Optional<Path> result = UserPrompts.promptFile(fileChooser).map(File::toPath);
		result.ifPresent(inpath -> {
			LOGGER.info("Will read batch job data from \"{}\".", inpath);
			settings.setInpath(inpath.toString());

			ForkJoinPool.commonPool().submit(() -> {
				LOGGER.debug("Saving class settings to \"{}\".", CLASS_SETTINGS_INFILE_PATH);
				try (OutputStream settingsOutStream = Files.newOutputStream(CLASS_SETTINGS_INFILE_PATH,
						StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
					settings.getProperties().store(settingsOutStream, String
							.format("Persisted settings for class \"%s\".", SessionEventLogAdjuster.class.getName()));
				} catch (final IOException e) {
					LOGGER.error(String.format("A(n) %s occurred while persisting class settings to file.",
							e.getClass().getSimpleName()), e);
				}
			});
		});
		return result;
	}

	private static void setColumnEventAttributeRenderers(final TableColumnModel colModel) {
		final EventAttribute[] attrs = EventAttribute.values();
		for (int i = 0; i < attrs.length; ++i) {
			final EventAttribute attr = attrs[i];
			final TableColumn col = colModel.getColumn(i);
			switch (attr) {
			case SENDER: {
				col.setCellRenderer(new EventDialogueFirstEventSenderRenderer());
				break;
			}
			case TIME: {
				col.setCellRenderer(new EventDialogueFirstEventTimeRenderer());
				break;
			}
			default: {
				throw new AssertionError("No logic for handing switch case.");
			}
			}
		}
	}

	private static void setMaxPreferredScrollableViewportSize(final JTable table) {
		final Dimension diagTablereferredScrollableViewportSize = table.getPreferredScrollableViewportSize();
		// http://stackoverflow.com/a/1936582/1391325
		final Dimension screenSize = table.getToolkit().getScreenSize();
		final double scaleFactor = 1.0;
		final Dimension maxPreferredSize = new Dimension(Math.toIntExact(Math.round(screenSize.width * scaleFactor)),
				Math.toIntExact(Math.round(screenSize.height * scaleFactor)));
		if (diagTablereferredScrollableViewportSize.width < maxPreferredSize.width) {
			diagTablereferredScrollableViewportSize.width = maxPreferredSize.width;
		}
		table.setPreferredScrollableViewportSize(diagTablereferredScrollableViewportSize);
	}

	private final ConcurrentMap<List<String>, String> tokenSeqReprs;

	private SessionEventLogAdjuster() {
		tokenSeqReprs = new ConcurrentHashMap<>(
				ESTIMATED_DIAGS_PER_SESSION * ESTIMATED_UNIQUE_TOKEN_SEQ_PER_DIAG_COUNT);
	}

	private void accept(final SessionDataManager sessionData) throws JAXBException, IOException {
		final PlayerDataManager playerData = sessionData.getPlayerData();
		final Table<String, String, GameHistory> gamePlayerHistoryTable = LoggedEvents
				.createPlayerGameHistoryTable(playerData.getPlayerEventLogs().entrySet(), EVENT_FILTER);
		final Set<String> playerGameIdIntersection = new HashSet<>(gamePlayerHistoryTable.rowKeySet());
		gamePlayerHistoryTable.columnMap().values().stream().map(Map::keySet)
				.forEach(playerGameIdIntersection::retainAll);
		final int uniqueModelDescriptionCount = gamePlayerHistoryTable.values().size();
		final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory(
				new GameContextModelFactory(uniqueModelDescriptionCount), new ImageEdgeCounter());

		final List<Utterance> utts = SessionUtterances.createUtteranceList(sessionData);
		final Path canonicalEventLogPath = sessionData.getCanonicalEventLogPath();
		LOGGER.info("Reading canonical event log at \"{}\".", canonicalEventLogPath);
		final Map<String, GameHistory> canonicalGameHistories = LoggedEvents.readGameHistories(canonicalEventLogPath,
				EVENT_FILTER);
		for (final String gameId : playerGameIdIntersection) {
			final GameHistory history = canonicalGameHistories.get(gameId);
			if (history == null) {
				LOGGER.warn("No canonical history for game ID \"{}\".", gameId);
			} else {
				final String title = createHistoryTitleStr(gameId, history);
				visualize(title, history, utts);
			}
		}
	}

	private Map<EventAttribute, TableColumn> createEventAttributeColumnMap(final TableColumnModel colModel) {
		final Map<EventAttribute, TableColumn> result = new EnumMap<>(EventAttribute.class);
		final EventAttribute[] attrs = EventAttribute.values();
		for (final EventAttribute attr : EventAttribute.values()) {
			result.put(attr, getEventAttributeColumn(colModel, attr));
		}
		assert result.size() == attrs.length;
		return result;
	}

	private String fetchTokenSeqRepr(final List<String> tokenSeq) {
		return tokenSeqReprs.computeIfAbsent(tokenSeq, key -> key.stream().collect(WORD_JOINER));
	}

	private void visualize(final String title, final GameHistory history, final List<Utterance> utts) {
		final Stream<EventDialogue> diags = EVENT_DIAG_FACTORY.apply(utts.listIterator(), history);
		final EventDialogueAdjusterTable diagTable = new EventDialogueAdjusterTable(
				new EventDialogueTableModel(diags.toArray(EventDialogue[]::new)),
				new UtteranceCellRenderer(this::fetchTokenSeqRepr));
		setMaxPreferredScrollableViewportSize(diagTable);

		final EventDialogueAdjusterFrame frame = new EventDialogueAdjusterFrame(title, history.getStartTime(),
				diagTable);
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

}
