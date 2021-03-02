package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.variable.VariableHelper;
import org.cyk.utility.__kernel__.variable.VariableName;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.MenuQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfilePrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

public class PersistenceApiPrivilegeUnitTestDev extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
		VariableHelper.write(VariableName.KEYCLOAK_ENABLED, Boolean.TRUE);
		VariableHelper.write(VariableName.KEYCLOAK_SERVER_URL, "http://localhost:8230/auth");
		VariableHelper.write(VariableName.KEYCLOAK_REALM_NAME, "SIIB");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_IDENTIFIER, "admin-cli");
		VariableHelper.write(VariableName.KEYCLOAK_CLIENT_SECRET, "ee334d25-4f98-4a42-a1e5-6097e7494fb7");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_USERNAME, "mic-acteur-api");
		VariableHelper.write(VariableName.KEYCLOAK_CREDENTIAL_PASSWORD, "mic-@cteur-@pi@2O2o");
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void profile(){
		System.out.println(StringUtils.repeat("-", 10)+"Profiles"+StringUtils.repeat("-", 10));
		for(Profile profile : ProfileQuerier.getInstance().read()) {
			Collection<ProfilePrivilege> profilePrivileges = ProfilePrivilegeQuerier.getInstance().readByProfilesCodes(List.of(profile.getCode()));
			Integer count = CollectionHelper.getSize(profilePrivileges);
			if(NumberHelper.isEqualToZero(count))
				continue;
			Collection<Privilege> privileges = PrivilegeQuerier.getInstance().readVisibleByProfilesCodes(List.of(profile.getCode()));
			System.out.println("Profile "+profile.getCode()+", #Associations = "+count+" , #Visible = "+CollectionHelper.getSize(privileges));
		}
	}
	
	@Test
	public void menu(){
		System.out.println(StringUtils.repeat("-", 10)+"Menus"+StringUtils.repeat("-", 10));
		for(Menu menu : MenuQuerier.getInstance().readWithAll()) {
			System.out.println("Menu "+menu.getName()+", Profiles = "+menu.getProfilesAsString());
		}
	}
	
	/**/
	
	public void assertReadWhereFilter(String actorCode,String firstName,String lastNames,String profileCode,String visibleSectionCode
			,String visibleBudgetSpecializationUnitCode ,String...expectedCodes) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().addFilterFieldsValues(
				IdentityQuerier.PARAMETER_NAME_CODE,actorCode,IdentityQuerier.PARAMETER_NAME_FIRST_NAME,firstName,IdentityQuerier.PARAMETER_NAME_LAST_NAMES,lastNames);
		if(StringHelper.isNotBlank(profileCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_PROFILE_CODE,profileCode);
		if(StringHelper.isNotBlank(visibleSectionCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_VISIBLE_SECTION_CODE,visibleSectionCode);
		if(StringHelper.isNotBlank(visibleBudgetSpecializationUnitCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE,visibleBudgetSpecializationUnitCode);
		Collection<Actor> actors = ActorQuerier.getInstance().readWhereFilter(arguments);
		if(ArrayHelper.isEmpty(expectedCodes)) {
			assertThat(actors).as("actors found").isNull();
		}else {
			assertThat(actors).as("actors not found for parameters AC="+actorCode+", FN="+firstName+", LN="+lastNames+", PC="+profileCode+", VSC="+visibleSectionCode
					+", VUSB="+visibleBudgetSpecializationUnitCode).isNotNull();
			assertThat(actors.stream().map(x -> x.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
		}
	}
}