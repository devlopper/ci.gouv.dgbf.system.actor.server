package ci.gouv.dgbf.system.actor.server.deployment;

import java.io.Serializable;

import javax.servlet.ServletContext;
import javax.servlet.annotation.WebListener;

import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.variable.VariableHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.server.deployment.AbstractServletContextListener;

import ci.gouv.dgbf.system.actor.server.representation.impl.ApplicationScopeLifeCycleListener;

@WebListener
public class ServletContextListener extends AbstractServletContextListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(ServletContext context) {
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, ConfigurationHelper.getValueAsString("KEYCLOAK_REALM"));
		super.__initialize__(context);
		__inject__(ApplicationScopeLifeCycleListener.class).initialize(null);
		//be sure we can work with keycloak client
		//KeycloakClientGetter.getInstance().get();
	}

}
