package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiScopeUnitTest extends AbstractPersistenceUnitTest {
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
	public void scopeQuerier_readInvisibleSectionsWhereFilter(){
		assertInvisibleSectionsByActorCode("a@m.com","section1","section2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("aa@m.com","section1","section2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_nothing","section1","section2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_section1","section2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_section1And2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_section1And3","section2","section4","section5");
		assertInvisibleSectionsByActorCode("kycdev@gmail.com","section2","section4");
	}
	
	@Test
	public void scopeQuerier_readVisibleSectionsWhereFilter(){
		assertVisibleSectionsByActorCode("a@m.com");
		assertVisibleSectionsByActorCode("aa@m.com");
		assertVisibleSectionsByActorCode("ACT_nothing");
		assertVisibleSectionsByActorCode("ACT_section1","section1");
		assertVisibleSectionsByActorCode("ACT_section1And2","section1","section2");
		assertVisibleSectionsByActorCode("ACT_section1And3","section1","section3");		
		assertVisibleSectionsByActorCode("kycdev@gmail.com","section1","section3","section5");
	}
	
	@Test
	public void scopeQuerier_readInvisibleAdministrativeUnitsWhereFilter(){
		assertInvisibleAdministrativeUnitsByActorCode("ACT_nothing","ua1section1","ua1section2","ua1section3","ua1section4","ua1section5");
		assertInvisibleAdministrativeUnitsByActorCode("ACT_section1","ua1section2","ua1section3","ua1section4","ua1section5");
		assertInvisibleAdministrativeUnitsByActorCode("ACT_section1And2","ua1section3","ua1section4","ua1section5");
		assertInvisibleAdministrativeUnitsByActorCode("ACT_section1And3","ua1section2","ua1section4","ua1section5");
	}
	
	@Test
	public void scopeQuerier_readVisibleAdministrativeUnitsWhereFilter(){
		assertVisibleAdministrativeUnitsByActorCode("ACT_nothing");
		assertVisibleAdministrativeUnitsByActorCode("ACT_section1","ua1section1","ua2section1","ua3section1");
		assertVisibleAdministrativeUnitsByActorCode("ACT_section1And2","ua1section1","ua1section2");
		assertVisibleAdministrativeUnitsByActorCode("ACT_section1And3","ua1section1","ua1section3");
	}
	
	@Test
	public void scopeQuerier_readInvisibleBudgetSpecializationUnitsWhereFilter(){
		assertInvisibleBudgetSpecializationUnitsByActorCode("ACT_nothing","usb1section1","usb1section2","usb1section3","usb1section4","usb1section5");
		assertInvisibleBudgetSpecializationUnitsByActorCode("ACT_section1","usb1section2","usb1section3","usb1section4","usb1section5");
		assertInvisibleBudgetSpecializationUnitsByActorCode("ACT_section1And2","usb1section3","usb1section4","usb1section5");
		assertInvisibleBudgetSpecializationUnitsByActorCode("ACT_section1And3","usb1section2","usb1section4","usb1section5");
	}
	
	@Test
	public void scopeQuerier_readVisibleBudgetSpecializationUnitsWhereFilter(){
		assertVisibleBudgetSpecializationUnitsByActorCode("ACT_nothing");
		assertVisibleBudgetSpecializationUnitsByActorCode("ACT_section1","usb1section1","usb2section1","usb3section1");
		assertVisibleBudgetSpecializationUnitsByActorCode("ACT_section1And2","usb1section1","usb1section2");
		assertVisibleBudgetSpecializationUnitsByActorCode("ACT_section1And3","usb1section1","usb1section3");
	}
	
	/**/

	private void assertInvisibleSectionsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeSectionQuerier.getInstance().readInvisibleWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertVisibleSectionsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeSectionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertInvisibleAdministrativeUnitsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readInvisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertVisibleAdministrativeUnitsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readVisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertInvisibleBudgetSpecializationUnitsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readInvisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeBudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertVisibleBudgetSpecializationUnitsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readVisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertScopes(Collection<Scope> scopes,String...expectedCodes) {
		if(CollectionHelper.isEmpty(scopes)) {
			assertThat(ArrayHelper.isEmpty(expectedCodes)).as("No scopes found").isTrue();
		}else {
			assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).contains(expectedCodes);
		}
	}
	
	/**/
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new ScopeType().setCode(ScopeType.CODE_SECTION),new ScopeType().setCode(ScopeType.CODE_UA)
				,new ScopeType().setCode(ScopeType.CODE_USB));
		for(Integer sectionIndex = 1; sectionIndex <= 5; sectionIndex = sectionIndex + 1) {
			Scope sectionScope = new Scope().setCode("section"+sectionIndex).setTypeFromIdentifier(ScopeType.CODE_SECTION);
			EntityCreator.getInstance().createManyInTransaction(sectionScope);
			Section section = new Section().setIdentifier(sectionScope.getIdentifier());
			EntityCreator.getInstance().createManyInTransaction(section);
			for(Integer uaIndex = 1; uaIndex <= 3; uaIndex = uaIndex + 1) {
				Scope administrativeUnitScope = new Scope().setCode("ua"+uaIndex+sectionScope.getCode()).setTypeFromIdentifier(ScopeType.CODE_UA);
				AdministrativeUnit administrativeUnit = new AdministrativeUnit().setIdentifier(administrativeUnitScope.getIdentifier())
						.setCode(administrativeUnitScope.getCode()).setSection(section);
				EntityCreator.getInstance().createManyInTransaction(administrativeUnitScope,administrativeUnit);
			}
			for(Integer usbIndex = 1; usbIndex <= 3; usbIndex = usbIndex + 1) {
				Scope usbScope = new Scope().setCode("usb"+usbIndex+sectionScope.getCode()).setTypeFromIdentifier(ScopeType.CODE_USB);
				BudgetSpecializationUnit usb = new BudgetSpecializationUnit().setCode(usbScope.getCode()).setSectionFromIdentifier(sectionScope.getIdentifier()); 
				EntityCreator.getInstance().createManyInTransaction(usbScope,usb);
			}
		}
		
		createActor("a@m.com");
		createActor("aa@m.com");
		
		createActor("ACT_nothing");
		createActor("ACT_section1","section1");
		createActor("ACT_section2","section2");
		createActor("ACT_section1And2","section1","section2");
		createActor("ACT_section1And3","section1","section3");
		
		createActor("kycdev@gmail.com","section1","ua2section3","usb3section5");
	}
	
	private void createActor(String code,String...scopesIdentifiers) {
		Identity identity = new Identity().setIdentifier(code).setFirstName(code).setLastNames(code).setElectronicMailAddress(code);
		EntityCreator.getInstance().createManyInTransaction(identity);
		Actor actor = new Actor().setCode(code).setIdentityFromIdentifier(identity.getIdentifier());
		EntityCreator.getInstance().createManyInTransaction(actor);
		if(ArrayHelper.isNotEmpty(scopesIdentifiers))
			for(String scopesIdentifier : scopesIdentifiers)
				EntityCreator.getInstance().createManyInTransaction(new ActorScope().setActorFromIdentifier(actor.getIdentifier()).setScopeFromIdentifier(scopesIdentifier));
	}
}