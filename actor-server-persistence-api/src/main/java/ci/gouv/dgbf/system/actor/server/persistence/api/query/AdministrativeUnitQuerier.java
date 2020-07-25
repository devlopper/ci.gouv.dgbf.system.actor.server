package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface AdministrativeUnitQuerier extends Querier {

	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	/* read where code or name like order by code ascending */
	String QUERY_NAME_READ_WHERE_CODE_OR_NAME_LIKE = "readWhereCodeOrNameLike";
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, QUERY_NAME_READ_WHERE_CODE_OR_NAME_LIKE);
	String QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE_FROM_WHERE = 
			Language.From.of("AdministrativeUnit t"
					+ " JOIN Scope scope ON scope = t ")
			+Language.Where.of(Language.Where.or(
		Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
		,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)));
	String QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE = Language.of(Language.Select.of("t.identifier,scope.code,scope.name"),QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE_FROM_WHERE,Language.Order.of("scope.code ASC"));
	Collection<AdministrativeUnit> readWhereCodeOrNameLike(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE);
	String QUERY_VALUE_COUNT_WHERE_CODE_OR_NAME_LIKE = Language.of(Language.Select.of("COUNT(t.identifier)"),QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE_FROM_WHERE);
	Long countWhereCodeOrNameLike(QueryExecutorArguments arguments);
		
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements AdministrativeUnitQuerier,Serializable {
		
		@Override
		public Collection<AdministrativeUnit> readWhereCodeOrNameLike(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, arguments);
		}
		
		@Override
		public Long countWhereCodeOrNameLike(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	static AdministrativeUnitQuerier getInstance() {
		return Helper.getInstance(AdministrativeUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE
				,Query.FIELD_TUPLE_CLASS,AdministrativeUnit.class,Query.FIELD_RESULT_CLASS,AdministrativeUnit.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME))
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE
				,Query.FIELD_TUPLE_CLASS,AdministrativeUnit.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_CODE_OR_NAME_LIKE
				)
			);		
	}
}