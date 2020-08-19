package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiActorUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void readProfileInformationsByCode(){
		Actor actor = ActorQuerier.getInstance().readProfileInformationsByCode("admin");
		assertThat(actor.getCivilityAsString()).isEqualTo("Monsieur");
		assertThat(actor.getFirstName()).isEqualTo("Komenan");
		assertThat(actor.getLastNames()).isEqualTo("Yao");		
		assertThat(actor.getGroupAsString()).isEqualTo("Fonctionnaire");
		assertThat(actor.getElectronicMailAddress()).isEqualTo("ky@m.com");
		assertThat(actor.getAdministrativeUnitAsString()).isEqualTo("1022 DTI");
		assertThat(actor.getSectionAsString()).isEqualTo("101 Representation Nationale");
		assertThat(actor.getAdministrativeFunction()).isEqualTo("Chef de service");
	}
	
	/**/
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(
				new ScopeType().setCode(ScopeType.CODE_SECTION).setOrderNumber((byte)1)
				,new ScopeType().setCode(ScopeType.CODE_USB).setOrderNumber((byte)2)
				,new ScopeType().setCode(ScopeType.CODE_ACTIVITE).setOrderNumber((byte)3)
				,new ScopeType().setCode(ScopeType.CODE_IMPUTATION).setOrderNumber((byte)4)
				,new ScopeType().setCode(ScopeType.CODE_UA).setOrderNumber((byte)5)
				,new Civility().setCode("Monsieur"),new Civility().setCode("Madame"),new Civility().setCode("Docteur")
				,new IdentityGroup().setCode("Fonctionnaire"),new IdentityGroup().setCode("Contractuel"));
		
		//Sections
		createSection("101","Representation Nationale");
		createSection("327","Ministère du Budget");
		createSection("323","Ministère de l'intérieur");
		
		//UAs
		createAdministrativeUnit("1022","DTI", "101");
		
		//Actors
		createActor("admin","Monsieur","Komenan","Yao","Fonctionnaire","ky@m.com","Chef de service","1022");		
	}
	
	private void createSection(String code,String name) {
		Scope sectionScope = new Scope().setCode(code).setName(name).setTypeFromIdentifier(ScopeType.CODE_SECTION);
		EntityCreator.getInstance().createManyInTransaction(sectionScope);
		Section section = new Section().setCode(sectionScope.getIdentifier()).setName(name);
		EntityCreator.getInstance().createManyInTransaction(section);
	}
	
	private void createAdministrativeUnit(String code,String name,String sectionIdentifier) {
		Scope administrativeUnitScope = new Scope().setCode(code).setName(name).setTypeFromIdentifier(ScopeType.CODE_UA);
		AdministrativeUnit administrativeUnit = new AdministrativeUnit().setIdentifier(administrativeUnitScope.getIdentifier())
				.setCode(administrativeUnitScope.getCode()).setName(name).setSectionFromIdentifier(sectionIdentifier);
		EntityCreator.getInstance().createManyInTransaction(administrativeUnitScope,administrativeUnit);
	}
	
	private void createActor(String code,String civilityIdentifier,String firstName,String lastNames,String groupIdentifier,String electronicMailAddress,String administrativeFunction,String administrativeUnitIdentifier) {
		Identity identity = new Identity().setIdentifier(code).setFirstName(firstName).setLastNames(lastNames).setElectronicMailAddress(electronicMailAddress)
				.setAdministrativeFunction(administrativeFunction).setAdministrativeUnitFromIdentifier(administrativeUnitIdentifier)
				.setCivilityFromIdentifier(civilityIdentifier).setGroupFromIdentifier(groupIdentifier);
		EntityCreator.getInstance().createManyInTransaction(identity);
		Actor actor = new Actor().setCode(code).setIdentityFromIdentifier(identity.getIdentifier());
		EntityCreator.getInstance().createManyInTransaction(actor);
	}
}