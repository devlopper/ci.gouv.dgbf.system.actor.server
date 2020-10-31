package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.annotation.Oracle;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.EntitySaver;
import org.cyk.utility.__kernel__.persistence.PersistenceHelper;
import org.cyk.utility.__kernel__.persistence.query.CountQueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.NativeQueryStringBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryResultMapper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActivityCategoryQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.BudgetaryFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.CivilityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.MenuQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfilePrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ServiceQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

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
				,EntitySaver.class,CountQueryIdentifierGetter.class);
		DependencyInjection.setQualifierClassTo(Oracle.class,NativeQueryStringBuilder.class);
		PersistenceHelper.CLASS_PRIMARY_KEY_COLUMN_NAME.put(ScopeFunctionExecutionImputation.class, ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER);
		PersistenceHelper.CLASS_COLUMNS_NAMES.put(ScopeFunctionExecutionImputation.class, Set.of(ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER
				,ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION
				,ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION));
		PersistenceHelper.COLUMN_NAME_FIELD.put(ScopeFunctionExecutionImputation.class, Map.of(
				ScopeFunctionExecutionImputation.COLUMN_IDENTIFIER
				,FieldHelper.getByName(ScopeFunctionExecutionImputation.class, ScopeFunctionExecutionImputation.FIELD_IDENTIFIER)
				,ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION
				,FieldHelper.getByName(ScopeFunctionExecutionImputation.class, ScopeFunctionExecutionImputation.FIELD_SCOPE_FUNCTION)
				,ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION
				,FieldHelper.getByName(ScopeFunctionExecutionImputation.class, ScopeFunctionExecutionImputation.FIELD_EXECUTION_IMPUTATION)
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
		ActivityCategoryQuerier.initialize();
		
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
		
		AccountRequestQuerier.initialize();
		RejectedAccountRequestQuerier.initialize();
		
		CivilityQuerier.initialize();
		AdministrativeUnitQuerier.initialize();
		BudgetaryFunctionQuerier.initialize();
		
		ScopeFunctionExecutionImputationQuerier.initialize();
		ExecutionImputationQuerier.initialize();
	}
}