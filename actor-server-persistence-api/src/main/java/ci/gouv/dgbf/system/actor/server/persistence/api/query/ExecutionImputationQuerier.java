package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.like;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
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

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

public interface ExecutionImputationQuerier extends Querier.CodableAndNamable<ExecutionImputation> {

	String PARAMETER_NAME_SECTION_CODE_NAME = "sectionCodeName";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "budgetSpecializationUnitCodeName";
	String PARAMETER_NAME_ACTION_CODE_NAME = "actionCodeName";
	String PARAMETER_NAME_ACTIVITY_CODE_NAME = "activityCodeName";
	String PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME = "economicNatureCodeName";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_CODE_NAME = "administrativeUnitCodeName";
	
	String QUERY_IDENTIFIER_READ_BY_SYSTEM_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(ExecutionImputation.class, "readBySystemIdentifierForEdit");
	ExecutionImputation readBySystemIdentifierForEdit(String identifier);
	
	/* Read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(ExecutionImputation.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<ExecutionImputation> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/* Read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_WITH_ALL = QueryIdentifierBuilder.getInstance().build(ExecutionImputation.class, QueryName.READ_WHERE_FILTER.getValue()+"WithAll");
	Collection<ExecutionImputation> readWhereFilterWithAll(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER_WITH_ALL = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER_WITH_ALL);
	Long countWhereFilterWithAll(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ExecutionImputation> implements ExecutionImputationQuerier,Serializable {		
		@Override
		protected void ____setAll____(Collection<?> collection) {
			if(CollectionHelper.isEmpty(collection))
				return;
			Collection<ExecutionImputation> executionImputations = CollectionHelper.cast(ExecutionImputation.class, collection);
			__setScopeFunctionExecutionImputations__(executionImputations);			
		}
		
		protected void __setScopeFunctionExecutionImputations__(Collection<ExecutionImputation> executionImputations) {
			if(CollectionHelper.isEmpty(executionImputations))
				return;
			Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations = ScopeFunctionExecutionImputationQuerier.getInstance()
					.readByExecutionImputations(executionImputations);
			if(CollectionHelper.isEmpty(scopeFunctionExecutionImputations))
				return;
			for(ExecutionImputation executionImputation : executionImputations) {
				Collection<ScopeFunctionExecutionImputation> collection = scopeFunctionExecutionImputations.stream()
						.filter(x -> x.getExecutionImputation().equals(executionImputation)).collect(Collectors.toList());
				if(CollectionHelper.isEmpty(collection))
					continue;
				for(ScopeFunctionExecutionImputation index : collection) {
					//if(!index.getExecutionImputation().equals(executionImputation))
					//	continue;
					if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_CREDIT_MANAGER_HOLDER))
						executionImputation.getCreditManager(Boolean.TRUE).setHolder(index.getScopeFunction());
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_CREDIT_MANAGER_ASSISTANT))
						executionImputation.getCreditManager(Boolean.TRUE).setAssistant(index.getScopeFunction());
					
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_AUTHORIZING_OFFICER_HOLDER))
						executionImputation.getAuthorizingOfficer(Boolean.TRUE).setHolder(index.getScopeFunction());
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_AUTHORIZING_OFFICER_ASSISTANT))
						executionImputation.getAuthorizingOfficer(Boolean.TRUE).setAssistant(index.getScopeFunction());
					
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_FINANCIAL_CONTROLLER_HOLDER))
						executionImputation.getFinancialController(Boolean.TRUE).setHolder(index.getScopeFunction());
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_FINANCIAL_CONTROLLER_ASSISTANT))
						executionImputation.getFinancialController(Boolean.TRUE).setAssistant(index.getScopeFunction());
					
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_ACCOUNTING_HOLDER))
						executionImputation.getAccounting(Boolean.TRUE).setHolder(index.getScopeFunction());
					else if(index.getScopeFunction().getFunction().getCode().equals(Function.CODE_ACCOUNTING_ASSISTANT))
						executionImputation.getAccounting(Boolean.TRUE).setAssistant(index.getScopeFunction());
				}
			}
		}
		
		@Override
		public ExecutionImputation readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_BY_SYSTEM_IDENTIFIER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
				return readBySystemIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<ExecutionImputation> readMany(QueryExecutorArguments arguments) {
			if(QueryIdentifierGetter.getInstance().get(getKlass(), QueryName.READ_WHERE_FILTER).equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);			
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterWithAll(arguments);
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QueryIdentifierGetter.getInstance().get(getKlass(), QueryName.COUNT_WHERE_FILTER).equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilterWithAll(arguments);
			return super.count(arguments);
		}
		
		@Override
		public Collection<ExecutionImputation> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(ExecutionImputation.class, QueryName.READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(ExecutionImputation.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(ExecutionImputation.class, QueryName.COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTIVITY_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);	
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ADMINISTRATIVE_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<ExecutionImputation> readWhereFilterWithAll(QueryExecutorArguments arguments) {
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			Collection<ExecutionImputation> collection = readWhereFilter(arguments);
			____setAll____(collection);
			return collection;
		}
		
		@Override
		public Long countWhereFilterWithAll(QueryExecutorArguments arguments) {
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			Long count = countWhereFilter(arguments);
			return count;
		}
		
		@Override
		public ExecutionImputation readBySystemIdentifierForEdit(String identifier) {
			QueryExecutorArguments arguments = new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_SYSTEM_IDENTIFIER_FOR_EDIT)
					.addFilterField(PARAMETER_NAME_IDENTIFIER, identifier);
			ExecutionImputation executionImputation = QueryExecutor.getInstance().executeReadOne(ExecutionImputation.class, arguments);
			if(executionImputation == null)
				return null;
			__setScopeFunctionExecutionImputations__(List.of(executionImputation));
			
			executionImputation.setFunctions(FunctionQuerier.getInstance().readByBusinessIdentifiers(Function.class,
				List.of(Function.CODE_CREDIT_MANAGER_HOLDER,Function.CODE_AUTHORIZING_OFFICER_HOLDER,Function.CODE_FINANCIAL_CONTROLLER_HOLDER
						,Function.CODE_ACCOUNTING_HOLDER)));
			return executionImputation;
		}
		
		@Override
		protected Class<ExecutionImputation> getKlass() {
			return ExecutionImputation.class;
		}
	}
	
	/**/
	
	static ExecutionImputationQuerier getInstance() {
		return Helper.getInstance(ExecutionImputationQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ExecutionImputation.class);
		QueryHelper.addQueries(
			Query.buildSelect(ExecutionImputation.class, QueryIdentifierGetter.getInstance().get(ExecutionImputation.class, QueryName.READ_BY_SYSTEM_IDENTIFIERS)
			, "SELECT ei FROM ExecutionImputation ei WHERE ei.identifier IN :"+PARAMETER_NAME_IDENTIFIERS)
			
			,Query.buildSelect(ExecutionImputation.class, QueryIdentifierGetter.getInstance().get(ExecutionImputation.class, QueryName.READ)
					, "SELECT t FROM ExecutionImputation t")
			
			,Query.buildSelect(ExecutionImputation.class, QueryIdentifierGetter.getInstance().get(ExecutionImputation.class, QueryName.COUNT)
					, "SELECT COUNT(t.identifier) FROM ExecutionImputation t")
			
			,Query.buildSelect(ExecutionImputation.class, QUERY_IDENTIFIER_READ_BY_SYSTEM_IDENTIFIER_FOR_EDIT
					, jpql(select("t"),From.ofTuple(ExecutionImputation.class),where("t.identifier = :"+PARAMETER_NAME_IDENTIFIER)))
			
			,Query.buildSelect(ExecutionImputation.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
					, jpql(
							select("DISTINCT(t."+ExecutionImputation.FIELD_IDENTIFIER+")",fields("t"
									,ExecutionImputation.FIELD_SECTION_CODE_NAME,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME,ExecutionImputation.FIELD_ACTION_CODE_NAME
									,ExecutionImputation.FIELD_ACTIVITY_CODE_NAME,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME))
							,getReadWhereFilterFromWhere()
							,order(asc("t",ExecutionImputation.FIELD_ACTIVITY_CODE_NAME)+","+asc("t",ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME))
						)
					)
				.setTupleFieldsNamesIndexesFromFieldsNames(ExecutionImputation.FIELD_IDENTIFIER
						,ExecutionImputation.FIELD_SECTION_CODE_NAME,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME,ExecutionImputation.FIELD_ACTION_CODE_NAME
						,ExecutionImputation.FIELD_ACTIVITY_CODE_NAME,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME
						)
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER
					, jpql(
							select("COUNT(DISTINCT t.identifier)")
							,getReadWhereFilterFromWhere()
						)
				)
		);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(
				from(
					"ExecutionImputation t"
					//,getReadWhereFilterFromWhereLeftJoinFunction(Function.CODE_CREDIT_MANAGER_HOLDER)
					//,getReadWhereFilterFromWhereLeftJoinFunction(Function.CODE_AUTHORIZING_OFFICER_HOLDER)
					//,getReadWhereFilterFromWhereLeftJoinFunction(Function.CODE_FINANCIAL_CONTROLLER_HOLDER)
					//,getReadWhereFilterFromWhereLeftJoinFunction(Function.CODE_ACCOUNTING_HOLDER)
				)
				,where(and(
					like("t", ExecutionImputation.FIELD_CODE, PARAMETER_NAME_CODE)
					,like("t", ExecutionImputation.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t", ExecutionImputation.FIELD_SECTION_CODE_NAME, PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t", ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t", ExecutionImputation.FIELD_ACTION_CODE_NAME, PARAMETER_NAME_ACTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t", ExecutionImputation.FIELD_ACTIVITY_CODE_NAME, PARAMETER_NAME_ACTIVITY_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t", ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME, PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t", ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME, PARAMETER_NAME_ADMINISTRATIVE_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				))
			);
	}
	
	static String getReadWhereFilterFromWhereLeftJoinFunction(String functionCode) {
		return jpql(String.format("LEFT JOIN ScopeFunctionExecutionImputation sfei%1$s ON sfei%1$s.executionImputation = t",functionCode)
					,String.format("LEFT JOIN ScopeFunction sf%1$s ON sf%1$s = sfei%1$s.scopeFunction",functionCode)
					,String.format("LEFT JOIN Function f%1$s ON f%1$s = sf%1$s.function",functionCode)
				);
	}
}