package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Language.From;
import org.cyk.utility.persistence.query.Language.Order;
import org.cyk.utility.persistence.query.Language.Select;
import org.cyk.utility.persistence.query.Language.Where;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;

public interface RejectedAccountRequestQuerier extends Querier {

	String PARAMETER_NAME_FIRST_NAME = "firstName";
	String PARAMETER_NAME_LAST_NAMES = "lastNames";
	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	String PARAMETER_NAME_NAMES = "names";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME = 2;
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES = 4;
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RejectedAccountRequestQuerier,Serializable {
		
		@Override
		public Collection<RejectedAccountRequest> readWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(RejectedAccountRequest.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME, arguments);	
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES, arguments);	
			filter.addFieldContains(PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS, arguments);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	/* read by electronic mail address */
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(RejectedAccountRequest.class, "readByElectronicMailAddress");
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS_WHERE = Where.of(Where.equals("t", RejectedAccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS,PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS));
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS = Language.of(Select.of("t"),From.ofTuple(RejectedAccountRequest.class)
			,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS_WHERE
			,Order.of(Order.join(Order.asc("t", RejectedAccountRequest.FIELD_FIRST_NAME),Order.asc("t", RejectedAccountRequest.FIELD_LAST_NAMES))));
	
	/* count by electronic mail address */
	String QUERY_IDENTIFIER_COUNT_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_COUNT_BY_ELECTRONIC_MAIL_ADDRESS = Language.of(Select.of("COUNT(t.identifier)"),From.ofTuple(RejectedAccountRequest.class),QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS_WHERE);
	
	/* Read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(RejectedAccountRequest.class, "readWhereFilter");
	Map<String,Integer> QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(RejectedAccountRequest.FIELD_IDENTIFIER
			,RejectedAccountRequest.FIELD_FIRST_NAME,RejectedAccountRequest.FIELD_LAST_NAMES,RejectedAccountRequest.FIELD_NAMES,RejectedAccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS
			,RejectedAccountRequest.FIELD_DATE,RejectedAccountRequest.FIELD_REQUEST_DATE);
	String QUERY_VALUE_READ_WHERE_FILTER_WHERE = Where.of(Where.and(
			Where.like("t", RejectedAccountRequest.FIELD_FIRST_NAME, PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME)
			,Where.like("t", RejectedAccountRequest.FIELD_LAST_NAMES, PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES)
			,Where.like("t", RejectedAccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS, PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS)
			));
	String QUERY_VALUE_READ_WHERE_FILTER = Language.of(Select.of("t.identifier,t.firstName,t.lastNames,"
			+Select.concat("t", RejectedAccountRequest.FIELD_FIRST_NAME,RejectedAccountRequest.FIELD_LAST_NAMES)+",t."+RejectedAccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS
			+",t.date,t.requestDate")
			,From.ofTuple(RejectedAccountRequest.class),QUERY_VALUE_READ_WHERE_FILTER_WHERE
			,Order.of(Order.join(Order.asc("t", RejectedAccountRequest.FIELD_FIRST_NAME),Order.asc("t", RejectedAccountRequest.FIELD_LAST_NAMES))));
	Collection<RejectedAccountRequest> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	String QUERY_VALUE_COUNT_WHERE_FILTER = Language.of(Select.of("COUNT(t.identifier)"),From.ofTuple(RejectedAccountRequest.class),QUERY_VALUE_READ_WHERE_FILTER_WHERE);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	static RejectedAccountRequestQuerier getInstance() {
		return Helper.getInstance(RejectedAccountRequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,RejectedAccountRequest.class,Query.FIELD_RESULT_CLASS,RejectedAccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,RejectedAccountRequest.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,RejectedAccountRequest.class,Query.FIELD_RESULT_CLASS,RejectedAccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,RejectedAccountRequest.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
	}
}