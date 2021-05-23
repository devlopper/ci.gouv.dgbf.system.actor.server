package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.PersistenceHelper;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.procedure.ProcedureExecutor;
import org.cyk.utility.persistence.server.query.ReaderByCollection;
import org.cyk.utility.persistence.server.query.string.FromStringBuilder;
import org.cyk.utility.persistence.server.query.string.JoinStringBuilder;
import org.cyk.utility.persistence.server.query.string.SelectStringBuilder;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class AssignmentsQuerierImpl extends AssignmentsQuerier.AbstractImpl implements Serializable {
	
	public static void initialize() {
		QueryManager.getInstance().register(
				Query.buildDynamicSelect(QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY, Assignments.class)
				,Query.buildDynamicCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY)
			);
	}
	
	@Override
	public Assignments readOne(QueryExecutorArguments arguments) {
		if(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
			return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
		if(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readByIdentifierForUI(arguments);
		throw new RuntimeException("Not yet handled : "+arguments);
	}
	
	@Override
	public Collection<Assignments> readMany(QueryExecutorArguments arguments) {
		if(QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilter(arguments);
		if(QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilterUsingIdentifiersOnly(arguments);
		if(QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return readFullyAssignedWhereFilter(arguments);
		if(QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return readNotFullyAssignedWhereFilter(arguments);

		if(QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readFullyAssignedWhereFilterForUI(arguments);
		if(QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readNotFullyAssignedWhereFilterForUI(arguments);
		
		if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilterForApplyModel(arguments);
		if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilterForUI(arguments);
		if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
			return readWhereFilterForEdit(arguments);
		throw new RuntimeException("Not yet handled : "+arguments);
	}
	
	@Override
	public Long count(QueryExecutorArguments arguments) {
		if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return countWhereFilter(arguments);
		if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return countWhereFilterUsingIdentifiersOnly(arguments);
		if(QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return countFullyAssignedWhereFilter(arguments);
		if(QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
			return countNotFullyAssignedWhereFilter(arguments);
		/*if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
			return countWhereFilterWithAll(arguments);
		*/
		throw new RuntimeException("Not yet handled : "+arguments);
	}
	
	@Override
	public Assignments readByIdentifierForEdit(String identifier) {
		Assignments assignments = QueryExecutor.getInstance().executeReadOne(Assignments.class, new QueryExecutorArguments()
				.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT).addFilterField(PARAMETER_NAME_IDENTIFIER, identifier));
		return assignments;
	}
	
	@Override
	public Assignments readByIdentifierForUI(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI);
		Assignments assignments = QueryExecutor.getInstance().executeReadOne(Assignments.class, arguments);
		return assignments;
	}
	
	@Override
	public Collection<Assignments> readWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QueryName.READ_WHERE_FILTER);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
		prepareWhereFilter(arguments);
		return QueryExecutor.getInstance().executeReadMany(Assignments.class, arguments);
	}
	
	@Override
	public Long countWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QueryName.COUNT_WHERE_FILTER);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
		prepareWhereFilter(arguments);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	private void prepareWhereFilter(QueryExecutorArguments arguments) {
		Filter filter = new Filter();
		
		filter.addFieldsNullable(arguments
				,PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IS_NULL
				,PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IS_NOT_NULL
				
				,PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IS_NULL
				,PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IS_NOT_NULL
				
				,PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IS_NULL
				,PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IS_NOT_NULL
				
				,PARAMETER_NAME_ACCOUNTING_HOLDER_IS_NULL
				,PARAMETER_NAME_ACCOUNTING_HOLDER_IS_NOT_NULL
				
				,PARAMETER_NAME_ALL_HOLDERS_DEFINED
				,PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED
				
				,PARAMETER_NAME_REGION_IDENTIFIER
				,PARAMETER_NAME_DEPARTMENT_IDENTIFIER
				,PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER
				);
		
		prepareWhereFilter(filter, arguments);
		arguments.setFilter(filter);
	}
	
	private void prepareWhereFilter(Filter filter,QueryExecutorArguments arguments) {
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_SECTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTIVITY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_ECONOMIC_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);	
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_ADMINISTRATIVE_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTIVITY_CATEGORY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		filter.addFieldContainsStringOrWords(PARAMETER_NAME_EXPENDITURE_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		
		filter.addFieldsEquals(arguments, PARAMETER_NAME_REGION_IDENTIFIER,PARAMETER_NAME_DEPARTMENT_IDENTIFIER,PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER);
		
		prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_CREDIT_MANAGER_HOLDER);
		prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER);
		prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER);
		prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_ACCOUNTING_HOLDER);
	}
	
	private void prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(QueryExecutorArguments arguments,Filter filter,String parameterName) {
		filter.addFieldsNullable(arguments, parameterName);
		filter.addFieldContainsStringOrWords(parameterName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
	}

	@Override
	public Collection<Assignments> readWhereFilterForApplyModel(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL);
		return readWhereFilter(arguments);
	}
	
	@Override
	public Collection<Assignments> readWhereFilterForUI(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
		return readWhereFilter(arguments);
	}
	
	@Override
	public Collection<Assignments> readWhereFilterForEdit(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT);
		return readWhereFilter(arguments);
	}
	
	@Override
	public Collection<Assignments> readFullyAssignedWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER);
		prepareFullyAssignedWhereFilter(arguments);
		return QueryExecutor.getInstance().executeReadMany(Assignments.class, arguments);
	}
	
	@Override
	public Long countFullyAssignedWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER);
		prepareFullyAssignedWhereFilter(arguments);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	private void prepareFullyAssignedWhereFilter(QueryExecutorArguments arguments) {
		Filter filter = new Filter();
		prepareWhereFilter(filter, arguments);
		arguments.setFilter(filter);
	}
	
	@Override
	public Collection<Assignments> readNotFullyAssignedWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER);
		prepareFullyAssignedWhereFilter(arguments);
		return QueryExecutor.getInstance().executeReadMany(Assignments.class, arguments);
	}
	
	@Override
	public Long countNotFullyAssignedWhereFilter(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER);
		prepareFullyAssignedWhereFilter(arguments);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	@Override
	public Collection<Assignments> readFullyAssignedWhereFilterForUI(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI);
		return readFullyAssignedWhereFilter(arguments);
	}
	
	@Override
	public Collection<Assignments> readNotFullyAssignedWhereFilterForUI(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = new QueryExecutorArguments();
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI);
		return readNotFullyAssignedWhereFilter(arguments);
	}
	
	@Override
	public Collection<Assignments> readWhereFilterUsingIdentifiersOnly(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		return QueryExecutor.getInstance().executeReadMany(Assignments.class, arguments);
	}
	
	@Override
	public Long countWhereFilterUsingIdentifiersOnly(QueryExecutorArguments arguments) {
		if(arguments == null)
			arguments = QueryExecutorArguments.instantiate(Assignments.class, QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		if(arguments.getQuery() == null)
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		return QueryExecutor.getInstance().executeCount(arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Collection<Object[]> readAllPropertiesByIdentifiers(Collection<String> identifiers) {
		if(CollectionHelper.isEmpty(identifiers))
			return null;
		return new ReaderByCollection.AbstractImpl<String, Object[]>() {
			@Override
			protected Collection<Object[]> __read__(Collection<String> values) {
				return EntityManagerGetter.getInstance().get().createQuery(jpql(
						SelectStringBuilder.getInstance().build(new SelectStringBuilder.Projection().addFromTuple("t", Assignments.FIELD_IDENTIFIER)
								.addFromTuple("i", ExecutionImputation.FIELD_SECTION_CODE_NAME, ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME
										, ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, ExecutionImputation.FIELD_ACTION_CODE_NAME
										, ExecutionImputation.FIELD_ACTIVITY_CODE_NAME, ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME
										, ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE_NAME, ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE_NAME)
								.add(Language.Select.concat(Assignments.COLUMN_CREDIT_MANAGER_HOLDER, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_ACCOUNTING_HOLDER, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								.add(Language.Select.concat(Assignments.COLUMN_ACCOUNTING_ASSISTANT, ScopeFunction.FIELD_CODE,ScopeFunction.FIELD_NAME))
								)
						,FromStringBuilder.getInstance().build(new FromStringBuilder.Tuple("Assignments","t")
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setTupleName(PersistenceHelper.getEntityName(ExecutionImputation.class)).setVariableName("i")))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_CREDIT_MANAGER_HOLDER)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_CREDIT_MANAGER_HOLDER)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_CREDIT_MANAGER_ASSISTANT)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_ACCOUNTING_HOLDER)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_ACCOUNTING_HOLDER)))
								.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t")
										.setMasterFieldName(Assignments.FIELD_ACCOUNTING_ASSISTANT)
										.setTupleName(PersistenceHelper.getEntityName(ScopeFunction.class)).setVariableName(Assignments.COLUMN_ACCOUNTING_ASSISTANT)))
								)
						,WhereStringBuilder.getInstance().build(new WhereStringBuilder.Predicate().add("t.identifier IN :"+PARAMETER_NAME_IDENTIFIERS))))
						.setParameter(PARAMETER_NAME_IDENTIFIERS, values).getResultList();
			}
			
		}.read(identifiers);
	}
	
	@Override
	public Collection<Object[]> readAllPropertiesByIdentifiers(String... identifiers) {
		if(ArrayHelper.isEmpty(identifiers))
			return null;
		return readAllPropertiesByIdentifiers(CollectionHelper.listOf(identifiers));
	}
	
	@Override
	public Collection<Object[]> readAllProperties(Collection<Assignments> assignmentsCollection) {
		if(CollectionHelper.isEmpty(assignmentsCollection))
			return null;
		return readAllPropertiesByIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(assignmentsCollection));
	}
	
	@Override
	public Collection<Object[]> readAllProperties(Assignments... assignmentsCollection) {
		if(ArrayHelper.isEmpty(assignmentsCollection))
			return null;
		return readAllProperties(CollectionHelper.listOf(assignmentsCollection));
	}
	
	@Override
	public void clean(String actor, String functionality, String action, Date date) {
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CLEAN
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,actor
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,functionality
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION,action
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(date.getTime())
			);
	}
	
	@Override
	public void import_(String actor, String functionality, String actionCreate,String actionUpdate, Date date) {
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,actor
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,functionality
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION_CREATE,actionCreate
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION_UPDATE,actionUpdate
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(date.getTime())
			);
	}
	
	@Override
	public void importNews(String actor, String functionality, String action,Date date) {
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT_NEWS
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,actor
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,functionality
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION,action
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(date.getTime())
			);
	}
	
	@Override
	public void export(String actor, String functionality, String action, Date date) {
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_EXPORT
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,actor
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,functionality
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION,action
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(date.getTime())
			);
	}		
	
	/**/
}