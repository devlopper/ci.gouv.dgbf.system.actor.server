package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Order;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;

public interface AccountRequestQuerier extends Querier {

	String PARAMETER_NAME_FIRST_NAME = "firstName";
	String PARAMETER_NAME_LAST_NAMES = "lastNames";
	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME = 2;
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES = 4;
	
	public static abstract class AbstractImpl extends AbstractObject implements AccountRequestQuerier,Serializable {
		
		@Override
		public Collection<AccountRequest> readWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(AccountRequest.class, arguments);
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
	
	/* Read by electronic mail address */
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readByElectronicMailAddress");
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS = Language.of(Select.of("t"),From.ofTuple(AccountRequest.class)
			,Where.of(Where.equals("t.identity", AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS,PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS))
			,Order.of(Order.join(Order.asc("t.identity", AccountRequest.FIELD_FIRST_NAME),Order.asc("t.identity", AccountRequest.FIELD_LAST_NAMES))));
	
	/* Read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readWhereFilter");
	Map<String,Integer> QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(AccountRequest.FIELD_IDENTIFIER
			,AccountRequest.FIELD_NAMES,AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_READ_WHERE_FILTER_WHERE = Where.of(Where.and(
			Where.like("t.identity", AccountRequest.FIELD_FIRST_NAME, PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME)
			,Where.like("t.identity", AccountRequest.FIELD_LAST_NAMES, PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES)
			,Where.like("t.identity", AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS, PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS)
			));
	String QUERY_VALUE_READ_WHERE_FILTER = Language.of(Select.of("t.identifier,"+Select.concat("t.identity", AccountRequest.FIELD_FIRST_NAME,AccountRequest.FIELD_LAST_NAMES)+",t.identity."+AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS)
			,From.ofTuple(AccountRequest.class),QUERY_VALUE_READ_WHERE_FILTER_WHERE
			,Order.of(Order.join(Order.asc("t.identity", AccountRequest.FIELD_FIRST_NAME),Order.asc("t.identity", AccountRequest.FIELD_LAST_NAMES))));
	Collection<AccountRequest> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	String QUERY_VALUE_COUNT_WHERE_FILTER = Language.of(Select.of("COUNT(t.identifier)"),From.ofTuple(AccountRequest.class),QUERY_VALUE_READ_WHERE_FILTER_WHERE);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	static AccountRequestQuerier getInstance() {
		return Helper.getInstance(AccountRequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
	}
}