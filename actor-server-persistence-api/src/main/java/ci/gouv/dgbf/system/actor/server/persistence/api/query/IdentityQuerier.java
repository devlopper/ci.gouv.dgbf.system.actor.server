package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.constant.ConstantEmpty;
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

import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public interface IdentityQuerier extends Querier {

	String PARAMETER_NAME_FIRST_NAME = "firstName";
	String PARAMETER_NAME_LAST_NAMES = "lastNames";
	String PARAMETER_NAME_NAMES = "names";
	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME = 2;
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES = 4;
	
	public static abstract class AbstractImpl extends AbstractObject implements IdentityQuerier,Serializable {
		
		@Override
		public Collection<Identity> readWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Identity.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		public static Filter buildFilterOfWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME, arguments);	
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES, arguments);	
			filter.addFieldContains(PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS, arguments);
			return filter;
		}
		
		public static void prepareWhereFilter(QueryExecutorArguments arguments) {
			arguments.setFilter(buildFilterOfWhereFilter(arguments));
		}
	}
	
	/**/
	
	/* Read by electronic mail address */
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(Identity.class, "readByElectronicMailAddress");
	static String getQueryValueReadByElectronicMailAddress(Class<?> tupleClass) {
		String variable = "t"+(Identity.class.equals(tupleClass) ? ConstantEmpty.STRING : ".identity");
		return Language.of(Select.of("t"),From.ofTuple(tupleClass)
				,Where.of(Where.equals(variable, Identity.FIELD_ELECTRONIC_MAIL_ADDRESS,PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS))
				,Order.of(Order.join(Order.asc(variable, Identity.FIELD_FIRST_NAME),Order.asc(variable, Identity.FIELD_LAST_NAMES))));
	}
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS = getQueryValueReadByElectronicMailAddress(Identity.class);
	
	/* read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Identity.class, "readWhereFilter");
	Map<String,Integer> QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Identity.FIELD_IDENTIFIER
			,Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES,Identity.FIELD_NAMES,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS);
	
	static String getQueryValueReadWhereFilterWhere(Class<?> tupleClass,Collection<String> additionalPredicates) {
		String variable = "t"+(Identity.class.equals(tupleClass) ? ConstantEmpty.STRING : ".identity");
		Collection<String> predicates = CollectionHelper.listOf(Where.like(variable, Identity.FIELD_FIRST_NAME, PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME)
				,Where.like(variable, Identity.FIELD_LAST_NAMES, PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES)
				,Where.like(variable, Identity.FIELD_ELECTRONIC_MAIL_ADDRESS, PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS));
		if(CollectionHelper.isNotEmpty(additionalPredicates))
			predicates.addAll(additionalPredicates);
		return Where.of(Where.and(predicates));
	}
	
	static String getQueryValueReadWhereFilter(Class<?> tupleClass,Collection<String> additionalFieldsNames,Collection<String> additionalPredicates) {
		String variable = "t"+(Identity.class.equals(tupleClass) ? ConstantEmpty.STRING : ".identity");
		String additionalFieldsNamesAsString = CollectionHelper.isEmpty(additionalFieldsNames) ? ConstantEmpty.STRING : 
			","+additionalFieldsNames.stream().map(x -> "t."+x).collect(Collectors.joining(","));
		return Language.of(Select.of("t.identifier,"+Select.fields(variable,Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES)
				+","+Select.concat(variable, Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES)
				+","+variable+"."+Identity.FIELD_ELECTRONIC_MAIL_ADDRESS+additionalFieldsNamesAsString)
				,From.ofTuple(tupleClass),getQueryValueReadWhereFilterWhere(tupleClass,additionalPredicates)
				,Order.of(Order.join(Order.asc(variable, Identity.FIELD_FIRST_NAME),Order.asc(variable, Identity.FIELD_LAST_NAMES))));
	}
	
	String QUERY_VALUE_READ_WHERE_FILTER = getQueryValueReadWhereFilter(Identity.class,null,null);
	Collection<Identity> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	static String getQueryValueCountWhereFilter(Class<?> tupleClass,Collection<String> additionalPredicates) {
		return Language.of(Select.of("COUNT(t.identifier)"),From.ofTuple(tupleClass),getQueryValueReadWhereFilterWhere(tupleClass,additionalPredicates));
	}
	String QUERY_VALUE_COUNT_WHERE_FILTER = getQueryValueCountWhereFilter(Identity.class,null);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	static IdentityQuerier getInstance() {
		return Helper.getInstance(IdentityQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Identity.class,Query.FIELD_RESULT_CLASS,Identity.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Identity.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,Identity.class,Query.FIELD_RESULT_CLASS,Identity.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
	}
}