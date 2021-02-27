package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.annotation.Oracle;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.query.EntitySaver;
import org.cyk.utility.persistence.PersistenceHelper;
import org.cyk.utility.persistence.query.CountQueryIdentifierGetter;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.NativeQueryStringBuilder;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryResultMapper;
import org.cyk.utility.persistence.server.TransientFieldsProcessor;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountingServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActivityCategoryQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AuthorizingOfficerServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.CivilityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.EconomicNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FinancialControllerServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationAttributeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormAttributeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentificationFormQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.MenuQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfilePrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestDispatchSlipQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestStatusQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.entities.ApplicationScopeLifeCycleListener.class).initialize(null);
	}
	
	@Override
	public void __destroy__(Object object) {}	

	public static void initialize() {
		ci.gouv.dgbf.system.actor.server.persistence.entities.ApplicationScopeLifeCycleListener.initialize();
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class,QueryResultMapper.class, EntityReader.class,EntityCounter.class
				,EntitySaver.class,CountQueryIdentifierGetter.class,TransientFieldsProcessor.class);
		DependencyInjection.setQualifierClassTo(Oracle.class,NativeQueryStringBuilder.class);
	
		
		PersistenceHelper.CLASS_COLUMNS_NAMES.put(Assignments.class, CollectionHelper.setOf(Assignments.COLUMN_IDENTIFIER,Assignments.COLUMN_EXECUTION_IMPUTATION
				,Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT
				,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT
				,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
				,Assignments.COLUMN_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_ASSISTANT));
		
		PersistenceHelper.COLUMN_NAME_FIELD.put(Assignments.class, Map.of(
				Assignments.COLUMN_IDENTIFIER,FieldHelper.getByName(Assignments.class, Assignments.FIELD_IDENTIFIER)
				
				,Assignments.COLUMN_EXECUTION_IMPUTATION,FieldHelper.getByName(Assignments.class, Assignments.FIELD_EXECUTION_IMPUTATION)
				
				,Assignments.COLUMN_CREDIT_MANAGER_HOLDER,FieldHelper.getByName(Assignments.class, Assignments.FIELD_CREDIT_MANAGER_HOLDER)
				,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT,FieldHelper.getByName(Assignments.class, Assignments.FIELD_CREDIT_MANAGER_ASSISTANT)
				
				,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,FieldHelper.getByName(Assignments.class, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
				,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT,FieldHelper.getByName(Assignments.class, Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT)
				
				,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,FieldHelper.getByName(Assignments.class, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
				,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT,FieldHelper.getByName(Assignments.class, Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT)
				
				,Assignments.COLUMN_ACCOUNTING_HOLDER,FieldHelper.getByName(Assignments.class, Assignments.FIELD_ACCOUNTING_HOLDER)
				,Assignments.COLUMN_ACCOUNTING_ASSISTANT,FieldHelper.getByName(Assignments.class, Assignments.FIELD_ACCOUNTING_ASSISTANT)
			));
		
		QueryHelper.scan(List.of(ActorQuerier.class.getPackage()));	
		
		IdentityQuerier.initialize();
		
		FunctionQuerier.initialize();
		FunctionTypeQuerier.initialize();
		
		ScopeQuerier.initialize();
		ScopeFunctionQuerier.initialize();
		ScopeTypeQuerier.initialize();
		ScopeTypeFunctionQuerier.initialize();
		SectionQuerier.initialize();
		BudgetSpecializationUnitQuerier.initialize();
		ActivityQuerier.initialize();
		ActivityCategoryQuerier.initialize();
		ExpenditureNatureQuerier.initialize();
		EconomicNatureQuerier.initialize();
		LocalityQuerier.initialize();
		AuthorizingOfficerServiceQuerier.initialize();
		FinancialControllerServiceQuerier.initialize();
		AccountingServiceQuerier.initialize();
		
		MenuQuerier.initialize();
		ServiceQuerier.initialize();
		PrivilegeQuerier.initialize();
		PrivilegeTypeQuerier.initialize();
		
		ProfileQuerier.initialize();
		ProfileTypeQuerier.initialize();
		ProfilePrivilegeQuerier.initialize();
		ProfileFunctionQuerier.initialize();
		
		ActorQuerier.initialize();
		ActorScopeQuerier.initialize();
		ActorProfileQuerier.initialize();
		
		RequestQuerier.initialize();
		RequestDispatchSlipQuerier.initialize();
		RequestStatusQuerier.initialize();
		RequestTypeQuerier.initialize();
		RequestFunctionQuerier.initialize();
		RequestScopeFunctionQuerier.initialize();
		
		AccountRequestQuerier.initialize();
		RejectedAccountRequestQuerier.initialize();
		
		CivilityQuerier.initialize();
		AdministrativeUnitQuerier.initialize();
		
		ExecutionImputationQuerier.initialize();
		AssignmentsQuerier.initialize();
		/*
		ClusterQuerier.initialize();
		ClusterPrivilegesQuerier.initialize();
		*/
		IdentificationAttributeQuerier.initialize();
		IdentificationFormQuerier.initialize();
		IdentificationFormAttributeQuerier.initialize();
	}
}