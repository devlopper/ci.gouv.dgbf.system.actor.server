package ci.gouv.dgbf.system.actor.server.persistence.entities;
import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.klass.PersistableClassesGetter;
import org.cyk.utility.server.persistence.AbstractApplicationScopeLifeCycleListenerEntities;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListenerEntities implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __destroy__(Object object) {}
	
	/**/
	
	public static void initialize() {
		PersistableClassesGetter.COLLECTION.set(List.of(
				ProfileFunction.class,ProfilePrivilege.class,Profile.class,ProfileType.class
				,Privilege.class,PrivilegeType.class
				,ScopeFunction.class,Scope.class,ScopeTypeFunction.class,ScopeType.class
				,Function.class,FunctionType.class
				,UserAccountProfile.class,UserAccount.class,User.class,Account.class
				));	
	}
}