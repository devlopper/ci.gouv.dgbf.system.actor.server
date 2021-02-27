package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;

public interface ExpenditureNatureQuerier extends Querier.CodableAndNamable<ExpenditureNature> {

	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeUnitIdentifier";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "budgetSpecializationUnitIdentifier";
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(ExpenditureNature.class, "readAllForUI");
	Collection<ExpenditureNature> readAllForUI();
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ExpenditureNature.class, "readBySectionIdentifierForUI");
	Collection<ExpenditureNature> readBySectionIdentifierForUI(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ExpenditureNature.class, "readByAdministrativeUnitIdentifierForUI");
	Collection<ExpenditureNature> readByAdministrativeUnitIdentifierForUI(String administrativeUnitIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ExpenditureNature.class, "readByBudgetSpecializationUnitIdentifierForUI");
	Collection<ExpenditureNature> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ExpenditureNature> implements ExpenditureNatureQuerier,Serializable {
	
		@Override
		public Collection<ExpenditureNature> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			if(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readBySectionIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByAdministrativeUnitIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByBudgetSpecializationUnitIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			return super.readMany(arguments);
		}
		
		@Override
		public Collection<ExpenditureNature> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(ExpenditureNature.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Collection<ExpenditureNature> readBySectionIdentifierForUI(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(ExpenditureNature.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					,PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<ExpenditureNature> readByAdministrativeUnitIdentifierForUI(String administrativeUnitIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(ExpenditureNature.class, QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI
					,PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER,administrativeUnitIdentifier);
		}
		
		@Override
		public Collection<ExpenditureNature> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(ExpenditureNature.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI
					,PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier);
		}
		
		@Override
		protected Class<ExpenditureNature> getKlass() {
			return ExpenditureNature.class;
		}
	}
	
	/**/
	
	static ExpenditureNatureQuerier getInstance() {
		return Helper.getInstance(ExpenditureNatureQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ExpenditureNature.class);
		QueryHelper.addQueries(
				Query.buildSelect(ExpenditureNature.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
				, "SELECT t.identifier,t.code,t.name FROM ExpenditureNature t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(ExpenditureNature.FIELD_IDENTIFIER,ExpenditureNature.FIELD_CODE,ExpenditureNature.FIELD_NAME)
				
				,Query.buildSelect(ExpenditureNature.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, "SELECT DISTINCT(t.identifier),t.code,t.name"
					+" FROM ExpenditureNature t"
					+" LEFT JOIN Activity a ON a.expenditureNature = t"
					+" LEFT JOIN Section s ON s = a.section"
					+" WHERE s.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER
					+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ExpenditureNature.FIELD_IDENTIFIER,ExpenditureNature.FIELD_CODE,ExpenditureNature.FIELD_NAME)
				
				,Query.buildSelect(ExpenditureNature.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI
					, "SELECT DISTINCT(t.identifier),t.code,t.name"
					+" FROM ExpenditureNature t"
					+" LEFT JOIN Activity a ON a.expenditureNature = t"
					+" LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit"
					+" WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
					+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ExpenditureNature.FIELD_IDENTIFIER,ExpenditureNature.FIELD_CODE,ExpenditureNature.FIELD_NAME)
					
				,Query.buildSelect(ExpenditureNature.class, QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI
					, "SELECT DISTINCT(t.identifier),t.code,t.name"
					+" FROM ExpenditureNature t"
					+" LEFT JOIN Activity a ON a.expenditureNature = t"
					+" LEFT JOIN AdministrativeUnit au ON au = a.administrativeUnit"
					+" WHERE au.identifier = :"+PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER
					+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ExpenditureNature.FIELD_IDENTIFIER,ExpenditureNature.FIELD_CODE,ExpenditureNature.FIELD_NAME)	
			);
	}
}