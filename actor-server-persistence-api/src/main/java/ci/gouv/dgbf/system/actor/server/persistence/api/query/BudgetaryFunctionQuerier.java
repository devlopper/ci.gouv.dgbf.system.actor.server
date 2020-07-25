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

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;

public interface BudgetaryFunctionQuerier extends Querier {

	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	/* read where code or name like order by code ascending */
	String QUERY_NAME_READ_WHERE_CODE_OR_NAME_LIKE = "readWhereCodeOrNameLike";
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE = QueryIdentifierBuilder.getInstance().build(BudgetaryFunction.class, QUERY_NAME_READ_WHERE_CODE_OR_NAME_LIKE);
	String QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE_FROM_WHERE = 
			Language.From.of("BudgetaryFunction t")
			+" "+Language.Where.of(Language.Where.or(
		Language.Where.like("t", BudgetaryFunction.FIELD_CODE, PARAMETER_NAME_CODE)
		,Language.Where.like("t", BudgetaryFunction.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)));
	String QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE = Language.of(Language.Select.of("t.identifier,t.code,t.name"),QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE_FROM_WHERE,Language.Order.of("t.code ASC"));
	Collection<BudgetaryFunction> readWhereCodeOrNameLike(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE);
	String QUERY_VALUE_COUNT_WHERE_CODE_OR_NAME_LIKE = Language.of(Language.Select.of("COUNT(t.identifier)"),QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE_FROM_WHERE);
	Long countWhereCodeOrNameLike(QueryExecutorArguments arguments);
		
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements BudgetaryFunctionQuerier,Serializable {
		
		@Override
		public Collection<BudgetaryFunction> readWhereCodeOrNameLike(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(BudgetaryFunction.class, arguments);
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
	
	static BudgetaryFunctionQuerier getInstance() {
		return Helper.getInstance(BudgetaryFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE
				,Query.FIELD_TUPLE_CLASS,BudgetaryFunction.class,Query.FIELD_RESULT_CLASS,BudgetaryFunction.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_CODE_OR_NAME_LIKE
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(BudgetaryFunction.FIELD_IDENTIFIER,BudgetaryFunction.FIELD_CODE,BudgetaryFunction.FIELD_NAME))
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE
				,Query.FIELD_TUPLE_CLASS,BudgetaryFunction.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_CODE_OR_NAME_LIKE
				)
			);		
	}
}