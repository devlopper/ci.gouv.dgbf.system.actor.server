package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.array.ArrayHelper;
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
import org.cyk.utility.__kernel__.string.StringHelper;
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
	
	static String getQueryValueReadWhereFilterWhere(Class<?> tupleClass,Collection<String> additionalPredicates) {
		String variable = "t"+(Identity.class.equals(tupleClass) ? ConstantEmpty.STRING : ".identity");
		Collection<String> predicates = CollectionHelper.listOf(
				Where.like(variable, Identity.FIELD_FIRST_NAME, PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME)
				,Where.like(variable, Identity.FIELD_LAST_NAMES, PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES)
				,Where.like(variable, Identity.FIELD_ELECTRONIC_MAIL_ADDRESS, PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS));
		if(CollectionHelper.isNotEmpty(additionalPredicates))
			predicates.addAll(additionalPredicates);
		return Where.of(Where.and(predicates));
	}
	
	static String getQueryValueReadWhereFilter(Class<?> tupleClass,Collection<String> additionalFieldsNames,String additionalJoins,Collection<String> additionalPredicates) {
		String variable = "t"+(Identity.class.equals(tupleClass) ? ConstantEmpty.STRING : ".identity");
		return jpql(
				//Select
				select(
				"t.identifier"
				,Select.fields(variable,Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES)
				,Select.concat(variable, Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES)
				,variable+"."+Identity.FIELD_ELECTRONIC_MAIL_ADDRESS
				,Select.concatCodeName("administrativeUnit")
				,variable+"."+Identity.FIELD_ADMINISTRATIVE_FUNCTION
				,Select.concatCodeName("section")
				,CollectionHelper.isEmpty(additionalFieldsNames) ? ConstantEmpty.STRING : additionalFieldsNames.stream().map(x -> "t."+x).collect(Collectors.joining(","))
				)
				//From
				,jpql(
					From.ofTuple(tupleClass)
					,"LEFT JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = "+variable+".administrativeUnit"
					,"LEFT JOIN Section section ON section = administrativeUnit.section"
					,(StringHelper.isBlank(additionalJoins) ? ConstantEmpty.STRING : " "+additionalJoins)
				)
				//Where
				,getQueryValueReadWhereFilterWhere(tupleClass,additionalPredicates)
				//Order
				,order(Order.join(asc(variable, Identity.FIELD_FIRST_NAME),asc(variable, Identity.FIELD_LAST_NAMES)))
				);
	}
	
	static Map<String,Integer> getQueryTupleFieldsNamesIndexesReadWhereFilter(Collection<String> additionalFieldsNames) {
		Collection<String> fieldsNames = CollectionHelper.listOf(Identity.FIELD_IDENTIFIER,Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES,Identity.FIELD_NAMES
				,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS,Identity.FIELD_ADMINISTRATIVE_UNIT_AS_STRING,Identity.FIELD_ADMINISTRATIVE_FUNCTION,Identity.FIELD_SECTION_AS_STRING);
		if(CollectionHelper.isNotEmpty(additionalFieldsNames))
			fieldsNames.addAll(additionalFieldsNames);
		return MapHelper.instantiateStringIntegerByStrings(fieldsNames);
	}
	
	static Map<String,Integer> getQueryTupleFieldsNamesIndexesReadWhereFilter(String...additionalFieldsNames) {
		Collection<String> collection;
		if(ArrayHelper.isEmpty(additionalFieldsNames))
			collection = null;
		else
			collection = List.of(additionalFieldsNames);
		return getQueryTupleFieldsNamesIndexesReadWhereFilter(collection);
	}
	
	String QUERY_VALUE_READ_WHERE_FILTER = getQueryValueReadWhereFilter(Identity.class,null,null,null);
	Collection<Identity> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	static String getQueryValueCountWhereFilter(Class<?> tupleClass,String additionalJoins,Collection<String> additionalPredicates) {
		return Language.of(Select.of("COUNT(t.identifier)"),From.ofTuple(tupleClass)+(StringHelper.isBlank(additionalJoins) ? ConstantEmpty.STRING : " "+additionalJoins)
				,getQueryValueReadWhereFilterWhere(tupleClass,additionalPredicates));
	}
	String QUERY_VALUE_COUNT_WHERE_FILTER = getQueryValueCountWhereFilter(Identity.class,null,null);
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
				).setTupleFieldsNamesIndexes(getQueryTupleFieldsNamesIndexesReadWhereFilter())
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