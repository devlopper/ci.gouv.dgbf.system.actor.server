package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.logging.Level;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class PersistenceScopeApiUnitTest extends AbstractPersistenceUnitTest {
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
	
	//@Test
	public void scopeQuerier_readInvisibleSectionsWhereFilter(){
		assertInvisibleSectionsByActorCode("ACT_nothing","section1","section2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_section1","section2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_section1And2","section3","section4","section5");
		assertInvisibleSectionsByActorCode("ACT_section1And3","section2","section4","section5");
	}
	
	//@Test
	public void scopeQuerier_readVisibleSectionsWhereFilter(){
		assertVisibleSectionsByActorCode("ACT_nothing");
		assertVisibleSectionsByActorCode("ACT_section1","section1");
		assertVisibleSectionsByActorCode("ACT_section1And2","section1","section2");
		assertVisibleSectionsByActorCode("ACT_section1And3","section1","section3");
	}
	
	@Test
	public void scopeQuerier_readInvisibleAdministrativeUnitsWhereFilter(){
		QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		for(AdministrativeUnit i : EntityReader.getInstance().readMany(AdministrativeUnit.class)) {
			System.out.println("Scope : "+EntityFinder.getInstance().find(Scope.class, i.getIdentifier()));
		}
		assertInvisibleAdministrativeUnitsByActorCode("ACT_nothing","ua1section1","ua1section2","ua1section3","ua1section4","ua1section5");
		//assertVisibleAdministrativeUnitsByActorCode("ACT_nothing");
		//assertInvisibleAdministrativeUnitsByActorCode("ACT_section1","section2","section3","section4","section5");
		//assertInvisibleAdministrativeUnitsByActorCode("ACT_section1And2","section3","section4","section5");
		//assertInvisibleAdministrativeUnitsByActorCode("ACT_section1And3","section2","section4","section5");
	}
	
	//@Test
	public void scopeQuerier_readInvisibleAdministrativeUnitsWithSectionsWhereFilter(){
		
	}

	private void assertInvisibleSectionsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeSectionQuerier.getInstance().readInvisibleSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertVisibleSectionsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeSectionQuerier.getInstance().readVisibleSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertInvisibleAdministrativeUnitsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readInvisibleAdministrativeUnitsWithSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertVisibleAdministrativeUnitsByActorCode(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readVisibleAdministrativeUnitsWithSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	private void assertScopes(Collection<Scope> scopes,String...expectedCodes) {
		if(CollectionHelper.isEmpty(scopes)) {
			assertThat(ArrayHelper.isEmpty(expectedCodes)).as("No scopes found").isTrue();
		}else {
			assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
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
			for(Integer uaIndex = 1; uaIndex <= 3; uaIndex = uaIndex + 1) {
				Scope administrativeUnitScope = new Scope().setCode("ua"+uaIndex+sectionScope.getCode()).setTypeFromIdentifier(ScopeType.CODE_UA);
				AdministrativeUnit administrativeUnit = new AdministrativeUnit().setIdentifier(administrativeUnitScope.getIdentifier())
						.setCode(administrativeUnitScope.getCode()).setSectionFromIdentifier(sectionScope.getCode());
				EntityCreator.getInstance().createManyInTransaction(administrativeUnitScope,administrativeUnit);
			}
			for(Integer usbIndex = 1; usbIndex <= 3; usbIndex = usbIndex + 1) {
				Scope usbScope = new Scope().setCode("usb"+usbIndex+sectionScope.getCode()).setTypeFromIdentifier(ScopeType.CODE_USB);
				BudgetSpecializationUnit usb = new BudgetSpecializationUnit().setCode(usbScope.getCode()).setSectionFromIdentifier(sectionScope.getCode()); 
				EntityCreator.getInstance().createManyInTransaction(usbScope,usb);
			}
		}
		
		EntityCreator.getInstance().createManyInTransaction(
				new Identity().setIdentifier("ACT_nothing").setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("m00")
				,new Identity().setIdentifier("ACT_section1").setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("m01")
				,new Identity().setIdentifier("ACT_section2").setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("m02")
				,new Identity().setIdentifier("ACT_section1And2").setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("m03")
				,new Identity().setIdentifier("ACT_section1And3").setFirstName("komenan").setLastNames("yao chrsitian").setElectronicMailAddress("m04")
		);
		
		EntityCreator.getInstance().createManyInTransaction(
				new Actor().setCode("ACT_nothing").setIdentityFromIdentifier("ACT_nothing")
				,new Actor().setCode("ACT_section1").setIdentityFromIdentifier("ACT_section1")
				,new Actor().setCode("ACT_section2").setIdentityFromIdentifier("ACT_section2")
				,new Actor().setCode("ACT_section1And2").setIdentityFromIdentifier("ACT_section1And2")
				,new Actor().setCode("ACT_section1And3").setIdentityFromIdentifier("ACT_section1And3")
		);

		EntityCreator.getInstance().createManyInTransaction(
			new ActorScope().setActorFromIdentifier("ACT_section1").setScopeFromIdentifier("section1")	
			,new ActorScope().setActorFromIdentifier("ACT_section1And2").setScopeFromIdentifier("section1")	
			,new ActorScope().setActorFromIdentifier("ACT_section1And2").setScopeFromIdentifier("section2")	
			,new ActorScope().setActorFromIdentifier("ACT_section1And3").setScopeFromIdentifier("section1")	
			,new ActorScope().setActorFromIdentifier("ACT_section1And3").setScopeFromIdentifier("section3")	
		);
	}
}