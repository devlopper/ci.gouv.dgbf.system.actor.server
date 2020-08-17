package ci.gouv.dgbf.system.actor.server.persistence.entities;
import java.io.Serializable;
import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.persistence.AbstractApplicationScopeLifeCycleListenerEntities;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListenerEntities implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __destroy__(Object object) {}
	
	/**/
	
	public static void initialize() {
		org.cyk.utility.__kernel__.klass.PersistableClassesGetter.COLLECTION.set(java.util.List.of(
				ProfileFunction.class,ProfilePrivilege.class
				,Privilege.class,PrivilegeType.class
				,ActorProfile.class,ActorScope.class
				,ActivityEconomicNature.class,AdministrativeUnit.class,Activity.class,BudgetSpecializationUnit.class,Section.class
				,ScopeFunction.class,Scope.class,ScopeTypeFunction.class
				,Function.class,FunctionType.class
				,Actor.class
				,Profile.class,ProfileType.class,ScopeType.class
				,AccountRequest.class
				,Identity.class
				,RejectedAccountRequest.class
				));	
	}
}