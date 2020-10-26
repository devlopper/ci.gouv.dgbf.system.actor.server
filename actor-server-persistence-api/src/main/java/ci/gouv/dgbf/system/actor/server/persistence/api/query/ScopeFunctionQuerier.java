package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.like;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Order;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionQuerier extends Querier.CodableAndNamable<ScopeFunction> {

	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_FUNCTION_CODE = "functionCode";
	String PARAMETER_NAME_FUNCTION_CODE_NULLABLE = PARAMETER_NAME_FUNCTION_CODE+"Nullable";
	
	String QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY = Querier.buildIdentifier(ScopeFunction.class, "readAllWithReferencesOnly");
	Collection<ScopeFunction> readAllWithReferencesOnly();
	
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readByFunctionsIdentifiers");
	Collection<ScopeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS);
	Long countByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = Querier.buildIdentifier(ScopeFunction.class, "readByFunctionsCodes");
	Collection<ScopeFunction> readByFunctionsCodes(QueryExecutorArguments arguments);
	Collection<ScopeFunction> readByFunctionsCodes(Collection<String> functionsCodes);
	Collection<ScopeFunction> readByFunctionsCodes(String...functionsCodes);
	
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES);
	Long countByFunctionsCodes(QueryExecutorArguments arguments);
	Long countByFunctionsCodes(Collection<String> functionsCodes);
	Long countByFunctionsCodes(String...functionsCodes);
	
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE = Querier.buildIdentifier(ScopeFunction.class, "readByFunctionCode");
	Collection<ScopeFunction> readWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE);
	Long countWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS = Querier.buildIdentifier(ScopeFunction.class, "readWithCodesOnlyByFunctionsIdentifiers");
	Collection<ScopeFunction> readWithCodesOnlyByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ_WHERE_FILTER);
	Collection<ScopeFunction> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.COUNT_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ScopeFunction> implements ScopeFunctionQuerier,Serializable {	
		
		@Override
		public Collection<ScopeFunction> readMany(QueryExecutorArguments arguments) {
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByFunctionsCodes(arguments);
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE.equals(arguments.getQuery().getIdentifier()))
				return readWhereCodeOrNameLikeByFunctionCode(arguments);
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
				return countByFunctionsCodes(arguments);
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE.equals(arguments.getQuery().getIdentifier()))
				return countWhereCodeOrNameLikeByFunctionCode(arguments);
			return super.count( arguments);
		}
		
		@Override
		public Collection<ScopeFunction> read() {
			return super.read(ScopeFunction.class, QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.READ));
		}
		
		@Override
		public Collection<ScopeFunction> readWithCodesOnlyByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
		}
		
		@Override
		public Collection<ScopeFunction> readByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
		}
		
		@Override
		public Long countByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS
					,PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers);
		}
		
		@Override
		public Collection<ScopeFunction> readByFunctionsCodes(QueryExecutorArguments arguments) {
			return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
		}
		
		@Override
		public Collection<ScopeFunction> readByFunctionsCodes(Collection<String> functionsCodes) {
			if(CollectionHelper.isEmpty(functionsCodes))
				return null;
			return readByFunctionsCodes(new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES)
					.addFilterFieldsValues(PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes));
		}
		
		@Override
		public Collection<ScopeFunction> readByFunctionsCodes(String... functionsCodes) {
			if(ArrayHelper.isEmpty(functionsCodes))
				return null;
			return readByFunctionsCodes(CollectionHelper.listOf(functionsCodes));
		}
		
		@Override
		public Long countByFunctionsCodes(QueryExecutorArguments arguments) {
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		@Override
		public Long countByFunctionsCodes(Collection<String> functionsCodes) {
			if(CollectionHelper.isEmpty(functionsCodes))
				return null;
			return countByFunctionsCodes(new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES)
					.addFilterFieldsValues(PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes));
		}
		
		@Override
		public Long countByFunctionsCodes(String... functionsCodes) {
			if(ArrayHelper.isEmpty(functionsCodes))
				return null;
			return countByFunctionsCodes(CollectionHelper.listOf(functionsCodes));
		}
		
		//
		
		@Override
		public Collection<ScopeFunction> readWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments) {
			prepareWhereCodeOrNameLikeByFunctionCode(arguments);
			return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
		}
		
		@Override
		public Long countWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments) {
			prepareWhereCodeOrNameLikeByFunctionCode(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		protected void prepareWhereCodeOrNameLikeByFunctionCode(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldEquals(PARAMETER_NAME_FUNCTION_CODE, arguments);
			arguments.setFilter(filter);
		}
		
		//
		
		@Override
		public Collection<ScopeFunction> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(ScopeFunction.class, QueryName.COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContains(PARAMETER_NAME_FUNCTION_CODE, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<ScopeFunction> readAllWithReferencesOnly() {
			return QueryExecutor.getInstance().executeReadMany(ScopeFunction.class, QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY);
		}
		
		@Override
		protected Class<ScopeFunction> getKlass() {
			return ScopeFunction.class;
		}
	}
	
	/**/
	
	static ScopeFunctionQuerier getInstance() {
		return Helper.getInstance(ScopeFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static String getQueryValueReadWhereFilterWhere() {
		return where(and(
				like("t", ScopeFunction.FIELD_CODE, PARAMETER_NAME_CODE)
				,like("t", ScopeFunction.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				,like("t", FieldHelper.join(ScopeFunction.FIELD_FUNCTION,Function.FIELD_CODE), PARAMETER_NAME_FUNCTION_CODE)
			));
	}
	
	static String getQueryValueReadWhereFilter() {
		return jpql(
				select(				
					Select.fields("t",ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_NUMBER_OF_ACTOR)
					,Select.concatCodeName(ScopeFunction.FIELD_SCOPE),Select.concatCodeName(ScopeFunction.FIELD_FUNCTION)
				)
				,jpql(
					From.ofTuple(ScopeFunction.class)
					,"LEFT JOIN Scope scope ON scope = t.scope"
					,"LEFT JOIN Function function ON function = t.function"
				)
				,getQueryValueReadWhereFilterWhere()
				,order(Order.join(asc("t",ScopeFunction.FIELD_CODE)))
			);
	}
	
	static String getQueryValueCountWhereFilter() {
		return jpql(select("COUNT(t.identifier)"),From.ofTuple(ScopeFunction.class),getQueryValueReadWhereFilterWhere());
	}
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ScopeFunction.class);
		
		QueryHelper.addQueries(
				Query.buildSelect(ScopeFunction.class, QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.READ)
						, "SELECT sf FROM ScopeFunction sf ORDER BY sf.code ASC")
				,Query.buildCount(QueryIdentifierGetter.getInstance().get(ScopeFunction.class, QueryName.COUNT)
						, "SELECT COUNT(sf.identifier) FROM ScopeFunction sf")
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_ALL_WITH_REFERENCES_ONLY
						, "SELECT t.identifier,t.code FROM ScopeFunction t").setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT sf FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT COUNT(sf) FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
						, "SELECT sf FROM ScopeFunction sf WHERE sf.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES
						, "SELECT COUNT(sf) FROM ScopeFunction sf WHERE sf.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE
						, getQueryValueReadWhereCodeOrNameLikeByFunctionCode())
						.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_FUNCTION_CODE
						, getQueryValueCountWhereCodeOrNameLikeByFunctionCode())
				
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT sf.code,sf.scope.code,sf.function.code FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
						.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
						, getQueryValueReadWhereFilter()).setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE
								,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_SHARED_AS_STRING,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, getQueryValueCountWhereFilter())
		);
	}
	
	/**/
	
	static String getQueryValueReadWhereCodeOrNameLikeByFunctionCodeFromWhere() {
		return jpql(
				from("ScopeFunction t")
				,where(and(
					parenthesis(or(
						like("t", ScopeFunction.FIELD_CODE, PARAMETER_NAME_CODE)
						,like("t", ScopeFunction.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					))
					,"t.function.code = :"+PARAMETER_NAME_FUNCTION_CODE
				))
			);
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByFunctionCode() {
		return jpql(select("t.identifier","t.code","t.name"),getQueryValueReadWhereCodeOrNameLikeByFunctionCodeFromWhere(),order("t.code ASC"));
	}
	
	
	static String getQueryValueCountWhereCodeOrNameLikeByFunctionCode() {
		return jpql(select("COUNT(t.identifier)"),getQueryValueReadWhereCodeOrNameLikeByFunctionCodeFromWhere());
	}
}