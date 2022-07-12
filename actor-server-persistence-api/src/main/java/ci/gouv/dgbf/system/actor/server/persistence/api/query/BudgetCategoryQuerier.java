package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetCategory;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface BudgetCategoryQuerier extends Querier.CodableAndNamable<BudgetCategory> {

	String PARAMETER_NAME_SEARCH = "search";
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(BudgetCategory.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(BudgetCategory.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(BudgetCategory.class, QueryName.COUNT_DYNAMIC);
	
	/* read order by code ascending */
	String QUERY_NAME_READ_ALL = "read";
	String QUERY_IDENTIFIER_READ_ALL = QueryIdentifierBuilder.getInstance().build(BudgetCategory.class, QUERY_NAME_READ_ALL);
	Collection<BudgetCategory> read();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(BudgetCategory.class, "readAllForUI");
	Collection<BudgetCategory> readAllForUI();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_ALL);
	Long count();
	
	String QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(BudgetCategory.class, "readVisiblesByActorCodeForUI");
	Collection<BudgetCategory> readVisiblesByActorCodeForUI(String actorCode/*,Boolean budgetSpecializationUnits,Boolean administrationUnits*/);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<BudgetCategory> implements BudgetCategoryQuerier,Serializable {
		@Override
		public Collection<BudgetCategory> read() {
			return QueryExecutor.getInstance().executeReadMany(BudgetCategory.class, QUERY_IDENTIFIER_READ_ALL);
		}
		
		@Override
		public Collection<BudgetCategory> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(BudgetCategory.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Collection<BudgetCategory> readVisiblesByActorCodeForUI(String actorCode/*,Boolean budgetSpecializationUnits,Boolean administrationUnits*/) {
			if(StringHelper.isBlank(actorCode))
				return null;
			Collection<Scope> scopes =  ScopeOfTypeBudgetCategoryQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ScopeOfTypeBudgetCategoryQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER)
					.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode));
			//Collection<Scope> scopes =  QueryExecutor.getInstance().executeReadMany(Scope.class, ScopeOfTypeBudgetCategoryQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER
			//		,ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);
			if(CollectionHelper.isEmpty(scopes))
				return null;
			return scopes.stream().map(x -> new BudgetCategory().setIdentifier(x.getIdentifier()).setCode(x.getCode()).setName(x.getName())).collect(Collectors.toList());
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public BudgetCategory readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(BudgetCategory.class,arguments.setQuery(null));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<BudgetCategory> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(BudgetCategory.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_READ_ALL.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			if(QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readVisiblesByActorCodeForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(BudgetCategory.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		protected Class<BudgetCategory> getKlass() {
			return BudgetCategory.class;
		}
	}
	
	/**/
	
	static BudgetCategoryQuerier getInstance() {
		return Helper.getInstance(BudgetCategoryQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(BudgetCategory.class);
		QueryManager.getInstance().register(
				Query.buildSelect(BudgetCategory.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI, "SELECT t.identifier,t.code,t.name FROM BudgetCategory t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(BudgetCategory.FIELD_IDENTIFIER,BudgetCategory.FIELD_CODE,BudgetCategory.FIELD_NAME)
		);		
	}
}