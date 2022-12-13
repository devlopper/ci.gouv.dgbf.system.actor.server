package ci.gouv.dgbf.system.actor.server.deployment;

import java.io.Serializable;

import javax.servlet.ServletContext;
import javax.servlet.annotation.WebListener;

import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.configuration.ConfigurationHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.variable.VariableHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.mail.MailSender;
import org.cyk.utility.security.keycloak.server.KeycloakClientGetter;
import org.cyk.utility.server.deployment.AbstractServletContextListener;

import ci.gouv.dgbf.system.actor.server.representation.impl.ApplicationScopeLifeCycleListener;

@WebListener
public class ServletContextListener extends AbstractServletContextListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(ServletContext context) {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
		
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, ConfigurationHelper.getValueAsString("KEYCLOAK_REALM"));
		
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_ENABLE, Boolean.TRUE);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_HOST, "smtp.postmarkapp.com");
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_PORT, 587);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_AUTHENTICATION_REQUIRED, Boolean.TRUE);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_SECURED_CONNECTION_REQUIRED, Boolean.TRUE);
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_AUTHENTICATION_CREDENTIALS_USER_IDENTIFIER, "6a3a9070-bd49-4d2d-a0fd-7e881a7b0e6a");
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_AUTHENTICATION_CREDENTIALS_USER_SECRET, "6a3a9070-bd49-4d2d-a0fd-7e881a7b0e6a");		
		VariableHelper.write(VariableName.PROTOCOL_SIMPLE_MAIL_TRANSFER_FROM, "no-reply@sigobe.dgbf.ci");
		
		KeycloakClientGetter.AbstractImpl.CONNECTION_POOL_SIZE = 100;
		//ClientManager.AbstractImpl.LOGGING_LEVEL = Level.INFO;
		super.__initialize__(context);
		__inject__(ApplicationScopeLifeCycleListener.class).initialize(null);
		DependencyInjection.setQualifierClass(MailSender.class, org.cyk.utility.mail.postmark.Postmark.class);
		//org.cyk.utility.persistence.server.QueryExecutor.AbstractImpl.LOGGABLE = Boolean.TRUE;
		//org.cyk.utility.persistence.server.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		//ClientManager.AbstractImpl.LOGGING_LEVEL = Level.INFO;
		
		String environment = ConfigurationHelper.getValueAsString("SIIB_ENVIRONMENT");
		
		LogHelper.logInfo(String.format("   ###   Environement   ### : %s", environment), getClass());
		
		if("dev".equals(environment)) {
			VariableHelper.write(VariableName.JASPER_SERVER_URL, "http://10.3.94.11:8001/jasperserver/");
		}else if("preproduction".equals(environment)) {
			
		}else if("debug".equals(environment)) {
			
		}else {
			VariableHelper.write(VariableName.JASPER_SERVER_URL, "http://10.3.4.24:8080/jasperserver/");
		}
		
		LogHelper.logInfo(String.format("   ###   Jasper   ### : %s", VariableHelper.read(VariableName.JASPER_SERVER_URL)), getClass());
	}
}