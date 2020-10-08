package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.like;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Order;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface ScopeFunctionQuerier extends Querier.CodableAndNamable<ScopeFunction> {

	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	
	String QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS = "readWithCodesOnlyByFunctionsIdentifiers";
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
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
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
				,order(Order.join(asc(ScopeFunction.FIELD_SCOPE, Scope.FIELD_CODE),asc(ScopeFunction.FIELD_FUNCTION, Function.FIELD_CODE)))
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
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WITH_CODES_ONLY_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT sf.code,sf.scope.code,sf.function.code FROM ScopeFunction sf WHERE sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS)
					.setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				,Query.buildSelect(ScopeFunction.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
						, getQueryValueReadWhereFilter()).setTupleFieldsNamesIndexesFromFieldsNames(ScopeFunction.FIELD_IDENTIFIER,ScopeFunction.FIELD_CODE
								,ScopeFunction.FIELD_NAME,ScopeFunction.FIELD_SHARED_AS_STRING,ScopeFunction.FIELD_SCOPE_AS_STRING,ScopeFunction.FIELD_FUNCTION_AS_STRING)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, getQueryValueCountWhereFilter())
		);
	}
}