package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import org.cyk.utility.__kernel__.security.keycloak.UserManager;
import org.cyk.utility.__kernel__.variable.VariableHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.server.persistence.test.arquillian.AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment;
import org.junit.Test;

public class BusinessIntegrationTestKeycloak extends AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		VariableHelper.write(VariableName.KEYCLOAK_ENABLED, Boolean.TRUE);
		VariableHelper.write(VariableName.KEYCLOAK_SERVER_URL, "http://10.3.4.60:8080/auth");
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, "SIIB");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_IDENTIFIER, "admin-cli");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_SECRET, "ac0749e3-14f8-4a14-a6e1-4edfb9375e69");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_USERNAME, "admin");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_PASSWORD, "admin");
	}
	
	//@Test
	public void readByUserName_admin_localhost() throws Exception{
		VariableHelper.write(VariableName.KEYCLOAK_ENABLED, Boolean.TRUE);
		VariableHelper.write(VariableName.KEYCLOAK_SERVER_URL, "http://localhost:8230/auth");
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, "test01");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_IDENTIFIER, "admin-cli");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_SECRET, "a8576f2d-762c-47ad-a973-26fac75e40ab");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_USERNAME, "admin");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_PASSWORD, "admin");
		System.out.println(UserManager.getInstance().readByUserName("admin"));
	}
	
	@Test
	public void readByUserName_admin_10_3_4_60() throws Exception{
		VariableHelper.write(VariableName.KEYCLOAK_ENABLED, Boolean.TRUE);
		VariableHelper.write(VariableName.KEYCLOAK_SERVER_URL, "http://10.3.4.60:8080/auth");
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, "SOLDE_TEST");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_IDENTIFIER, "admin-cli");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_SECRET, "2cec22db-bfae-42ba-92d0-59713da5d246");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_USERNAME, "admin");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_PASSWORD, "admin");
		try {
			System.out.println(UserManager.getInstance().readByUserName("admin"));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
