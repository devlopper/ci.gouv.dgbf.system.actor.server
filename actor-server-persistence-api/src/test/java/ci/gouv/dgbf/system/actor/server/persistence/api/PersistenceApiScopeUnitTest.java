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

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
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
	public void readTypeOrderByCodeAscending(){
		assertThat(ScopeTypeQuerier.getInstance().readOrderByCodeAscending().stream().map(x -> x.getCode()).collect(Collectors.toList()))
			.containsExactly(ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION,ScopeType.CODE_SECTION,ScopeType.CODE_UA,ScopeType.CODE_USB);
	}
	
	@Test
	public void readTypeOrderByOrderNumberAscending(){
		assertThat(ScopeTypeQuerier.getInstance().readOrderByOrderNumberAscending().stream().map(x -> x.getCode()).collect(Collectors.toList()))
			.containsExactly(ScopeType.CODE_SECTION,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION,ScopeType.CODE_UA);
	}
	
	@Test
	public void readInvisibleSectionsWhereFilter(){
		assertInvisibleSectionsByFilter("inconnu", null, null, "s1","s2","s3");
		assertInvisibleSectionsByFilter("inconnu", "s", null, "s1","s2","s3");
		assertInvisibleSectionsByFilter("inconnu", "s1", null,"s1");
		assertInvisibleSectionsByFilter("inconnu", "2", null,"s2");
		
		assertInvisibleSectionsByFilter("admin", null, null);
		
		assertInvisibleSectionsByFilter("section_manager_1", null, null, "s2","s3");
		assertInvisibleSectionsByFilter("section_manager_2", null, null, "s1","s3");
		assertInvisibleSectionsByFilter("section_manager_3", null, null, "s1","s2");
		assertInvisibleSectionsByFilter("section_manager_4", null, null, "s2");
		
		assertInvisibleSectionsByFilter("usb_manager_1", null, null, "s2","s3");
		assertInvisibleSectionsByFilter("usb_manager_2", null, null, "s2","s3");
		assertInvisibleSectionsByFilter("usb_manager_3", null, null,"s2");
		
		assertInvisibleSectionsByFilter("activity_manager_1", null, null, "s2","s3");
		assertInvisibleSectionsByFilter("activity_manager_2", null, null, "s2","s3");
		assertInvisibleSectionsByFilter("activity_manager_3", null, null, "s2");
		
		assertInvisibleSectionsByFilter("ua_manager_1", null, null, "s3");
		assertInvisibleSectionsByFilter("ua_manager_2", null, null, "s2","s3");
		assertInvisibleSectionsByFilter("ua_manager_3", null, null, "s1","s2");
	}
	
	@Test
	public void readVisibleSectionsWhereFilter(){
		assertVisibleSectionsByFilter("inconnu", null, null);
		assertVisibleSectionsByFilter("inconnu", "s", null);
		assertVisibleSectionsByFilter("inconnu", "s1", null);
		assertVisibleSectionsByFilter("inconnu", "2", null);
		
		assertVisibleSectionsByFilter("admin", null, null,"s1","s2","s3");
		assertVisibleSectionsByFilter("admin", "s", null,"s1","s2","s3");
		assertVisibleSectionsByFilter("admin", "s1", null,"s1");
		assertVisibleSectionsByFilter("admin", "2", null,"s2");
		
		assertVisibleSectionsByFilter("section_manager_1", null, null, "s1");
		assertVisibleSectionsByFilter("section_manager_2", null, null, "s2");
		assertVisibleSectionsByFilter("section_manager_3", null, null, "s3");
		assertVisibleSectionsByFilter("section_manager_4", null, null, "s1","s3");
		
		assertVisibleSectionsByFilter("usb_manager_1", null, null, "s1");
		assertVisibleSectionsByFilter("usb_manager_2", null, null, "s1");
		assertVisibleSectionsByFilter("usb_manager_3", null, null, "s1","s3");
		
		assertVisibleSectionsByFilter("activity_manager_1", null, null, "s1");
		assertVisibleSectionsByFilter("activity_manager_2", null, null, "s1");
		assertVisibleSectionsByFilter("activity_manager_3", null, null, "s1","s3");
		
		assertVisibleSectionsByFilter("ua_manager_1", null, null, "s1","s2");
		assertVisibleSectionsByFilter("ua_manager_2", null, null, "s1");
		assertVisibleSectionsByFilter("ua_manager_3", null, null, "s3");
	}
	
	@Test
	public void readInvisibleBudgetSpecializationUnitsWhereFilter(){
		assertInvisibleBudgetSpecializationUnitsByFilter("inconnu", null, null, "usb1","usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("inconnu", "s", null, "usb1","usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("inconnu", "s1", null);
		assertInvisibleBudgetSpecializationUnitsByFilter("inconnu", "2", null,"usb2");
		
		assertInvisibleBudgetSpecializationUnitsByFilter("admin", null, null);
		
		assertInvisibleBudgetSpecializationUnitsByFilter("section_manager_1", null, null, "usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("section_manager_2", null, null, "usb1","usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("section_manager_3", null, null, "usb1","usb2","usb3");
		assertInvisibleBudgetSpecializationUnitsByFilter("section_manager_4", null, null);
		
		assertInvisibleBudgetSpecializationUnitsByFilter("usb_manager_1", null, null, "usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("usb_manager_2", null, null, "usb1","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("usb_manager_3", null, null,"usb1","usb2");
		
		assertInvisibleBudgetSpecializationUnitsByFilter("activity_manager_1", null, null, "usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("activity_manager_2", null, null, "usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("activity_manager_3", null, null, "usb1","usb3","usb4");
		
		assertInvisibleBudgetSpecializationUnitsByFilter("ua_manager_1", null, null, "usb1","usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("ua_manager_2", null, null, "usb1","usb2","usb3","usb4","usb5");
		assertInvisibleBudgetSpecializationUnitsByFilter("ua_manager_3", null, null, "usb1","usb2","usb3","usb4","usb5");
	}
	
	@Test
	public void readVisibleBudgetSpecializationUnitsWhereFilter(){
		assertVisibleBudgetSpecializationUnitsByFilter("inconnu", null, null);
		assertVisibleBudgetSpecializationUnitsByFilter("inconnu", "s", null);
		assertVisibleBudgetSpecializationUnitsByFilter("inconnu", "s1", null);
		assertVisibleBudgetSpecializationUnitsByFilter("inconnu", "2", null);
		
		assertVisibleBudgetSpecializationUnitsByFilter("admin", null, null,"usb1","usb2","usb3","usb4","usb5");
		assertVisibleBudgetSpecializationUnitsByFilter("admin", "s", null,"usb1","usb2","usb3","usb4","usb5");
		assertVisibleBudgetSpecializationUnitsByFilter("admin", "s1", null);
		assertVisibleBudgetSpecializationUnitsByFilter("admin", "2", null,"usb2");
		
		assertVisibleBudgetSpecializationUnitsByFilter("section_manager_1", null, null, "usb1","usb2","usb3");
		assertVisibleBudgetSpecializationUnitsByFilter("section_manager_2", null, null);
		assertVisibleBudgetSpecializationUnitsByFilter("section_manager_3", null, null, "usb4","usb5");
		assertVisibleBudgetSpecializationUnitsByFilter("section_manager_4", null, null, "usb1","usb2","usb3","usb4","usb5");
		
		assertVisibleBudgetSpecializationUnitsByFilter("usb_manager_1", null, null, "usb1");
		assertVisibleBudgetSpecializationUnitsByFilter("usb_manager_2", null, null, "usb2");
		assertVisibleBudgetSpecializationUnitsByFilter("usb_manager_3", null, null, "usb3","usb4","usb5");
		
		assertVisibleBudgetSpecializationUnitsByFilter("activity_manager_1", null, null, "usb1");
		assertVisibleBudgetSpecializationUnitsByFilter("activity_manager_2", null, null, "usb1","usb2");
		assertVisibleBudgetSpecializationUnitsByFilter("activity_manager_3", null, null, "usb2","usb5");
		
		assertVisibleBudgetSpecializationUnitsByFilter("ua_manager_1", null, null);
		assertVisibleBudgetSpecializationUnitsByFilter("ua_manager_2", null, null);
		assertVisibleBudgetSpecializationUnitsByFilter("ua_manager_3", null, null);
	}
	
	@Test
	public void readInvisibleActionsWhereFilter(){		
		assertInvisibleActionsByFilter("inconnu", null, null,"action1","action2","action3","action4");
		assertInvisibleActionsByFilter("inconnu", "action", null,"action1","action2","action3","action4");
		assertInvisibleActionsByFilter("inconnu", "action1", null,"action1");
		assertInvisibleActionsByFilter("inconnu", "2", null,"action2");
		
		assertInvisibleActionsByFilter("admin", null, null);
		assertInvisibleActionsByFilter("admin", "action", null);
		assertInvisibleActionsByFilter("admin", "action1", null);
		assertInvisibleActionsByFilter("admin", "2", null);
		
		assertInvisibleActionsByFilter("section_manager_1", null, null, "action4");
		assertInvisibleActionsByFilter("section_manager_2", null, null,"action1","action2","action3","action4");
		assertInvisibleActionsByFilter("section_manager_3", null, null, "action1","action2","action3");
		assertInvisibleActionsByFilter("section_manager_4", null, null);
		
		assertInvisibleActionsByFilter("usb_manager_1", null, null, "action3","action4");
		assertInvisibleActionsByFilter("usb_manager_2", null, null, "action1","action2","action4");
		assertInvisibleActionsByFilter("usb_manager_3", null, null, "action1","action2","action3");
		
		assertInvisibleActionsByFilter("activity_manager_1", null, null, "action2","action3","action4");
		assertInvisibleActionsByFilter("activity_manager_2", null, null, "action1","action4");
		assertInvisibleActionsByFilter("activity_manager_3", null, null, "action1","action2");
		
		assertInvisibleActionsByFilter("ua_manager_1", null, null,"action1","action2","action3","action4");
		assertInvisibleActionsByFilter("ua_manager_2", null, null,"action1","action2","action3","action4");
		assertInvisibleActionsByFilter("ua_manager_3", null, null,"action1","action2","action3","action4");
		
	}
	
	@Test
	public void readVisibleActionsWhereFilter(){
		assertVisibleActionsByFilter("inconnu", null, null);
		assertVisibleActionsByFilter("inconnu", "s", null);
		assertVisibleActionsByFilter("inconnu", "s1", null);
		assertVisibleActionsByFilter("inconnu", "2", null);
		
		assertVisibleActionsByFilter("admin", null, null,"action1","action2","action3","action4");
		assertVisibleActionsByFilter("admin", "action", null,"action1","action2","action3","action4");
		assertVisibleActionsByFilter("admin", "action1", null,"action1");
		assertVisibleActionsByFilter("admin", "2", null,"action2");
		
		assertVisibleActionsByFilter("section_manager_1", null, null, "action1","action2","action3");
		assertVisibleActionsByFilter("section_manager_2", null, null);
		assertVisibleActionsByFilter("section_manager_3", null, null, "action4");
		assertVisibleActionsByFilter("section_manager_4", null, null, "action1","action2","action3","action4");
		
		assertVisibleActionsByFilter("usb_manager_1", null, null, "action1","action2");
		assertVisibleActionsByFilter("usb_manager_2", null, null, "action3");
		assertVisibleActionsByFilter("usb_manager_3", null, null, "action4");
		
		assertVisibleActionsByFilter("action_manager_1", null, null, "action1");
		
		assertVisibleActionsByFilter("activity_manager_1", null, null, "action1");
		assertVisibleActionsByFilter("activity_manager_2", null, null, "action2","action3");
		assertVisibleActionsByFilter("activity_manager_3", null, null, "action3","action4");
		
		assertVisibleActionsByFilter("ua_manager_1", null, null);
		assertVisibleActionsByFilter("ua_manager_2", null, null);
		assertVisibleActionsByFilter("ua_manager_3", null, null);
	}
	
	@Test
	public void readInvisibleActivitiessWhereFilter(){
		assertInvisibleActivitiesByFilter("inconnu", null, null, "a1","a2","a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("inconnu", "a", null, "a1","a2","a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("inconnu", "a1", null,"a1");
		assertInvisibleActivitiesByFilter("inconnu", "2", null,"a2");
		
		assertInvisibleActivitiesByFilter("admin", null, null);
		
		assertInvisibleActivitiesByFilter("section_manager_1", null, null, "a6");
		assertInvisibleActivitiesByFilter("section_manager_2", null, null, "a1","a2","a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("section_manager_3", null, null, "a1","a2","a3","a4","a5");
		assertInvisibleActivitiesByFilter("section_manager_4", null, null);
		
		assertInvisibleActivitiesByFilter("usb_manager_1", null, null, "a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("usb_manager_2", null, null,"a1","a2","a6");
		assertInvisibleActivitiesByFilter("usb_manager_3", null, null,"a1","a2","a3","a4","a5");
		
		assertInvisibleActivitiesByFilter("activity_manager_1", null, null, "a2","a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("activity_manager_2", null, null, "a1","a5","a6");
		assertInvisibleActivitiesByFilter("activity_manager_3", null, null, "a1","a2","a3","a4");
		
		assertInvisibleActivitiesByFilter("ua_manager_1", null, null, "a1","a2","a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("ua_manager_2", null, null, "a1","a2","a3","a4","a5","a6");
		assertInvisibleActivitiesByFilter("ua_manager_3", null, null, "a1","a2","a3","a4","a5","a6");
	}
	
	@Test
	public void readVisibleActivitiessWhereFilter(){
		assertVisibleActivitiesByFilter("inconnu", null, null);
		assertVisibleActivitiesByFilter("inconnu", "a", null);
		assertVisibleActivitiesByFilter("inconnu", "a1", null);
		assertVisibleActivitiesByFilter("inconnu", "2", null);
		
		assertVisibleActivitiesByFilter("admin", null, null,"a1","a2","a3","a4","a5","a6");
		assertVisibleActivitiesByFilter("admin", "a", null,"a1","a2","a3","a4","a5","a6");
		assertVisibleActivitiesByFilter("admin", "a1",null,"a1");
		assertVisibleActivitiesByFilter("admin", "2", null,"a2");
		
		assertVisibleActivitiesByFilter("section_manager_1", null, null, "a1","a2","a3","a4","a5");
		assertVisibleActivitiesByFilter("section_manager_2", null, null);
		assertVisibleActivitiesByFilter("section_manager_3", null, null, "a6");
		assertVisibleActivitiesByFilter("section_manager_4", null, null, "a1","a2","a3","a4","a5","a6");
		
		assertVisibleActivitiesByFilter("usb_manager_1", null, null, "a1","a2");
		assertVisibleActivitiesByFilter("usb_manager_2", null, null, "a3","a4","a5");
		assertVisibleActivitiesByFilter("usb_manager_3", null, null, "a6");
		
		assertVisibleActivitiesByFilter("activity_manager_1", null, null, "a1");
		assertVisibleActivitiesByFilter("activity_manager_2", null, null, "a2","a3","a4");
		assertVisibleActivitiesByFilter("activity_manager_3", null, null, "a5","a6");
		
		assertVisibleActivitiesByFilter("ua_manager_1", null, null);
		assertVisibleActivitiesByFilter("ua_manager_2", null, null);
		assertVisibleActivitiesByFilter("ua_manager_3", null, null);
	}
	
	@Test
	public void readInvisibleAdministrativeUnitsWhereFilter(){
		assertInvisibleAdministrativeUnitsByFilter("inconnu", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("inconnu", "a", null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("inconnu", "a1", null,"ua1");
		assertInvisibleAdministrativeUnitsByFilter("inconnu", "2", null,"ua2");
		
		assertInvisibleAdministrativeUnitsByFilter("admin", null, null);
		
		assertInvisibleAdministrativeUnitsByFilter("section_manager_1", null, null, "ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("section_manager_2", null, null, "ua1","ua2","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("section_manager_3", null, null, "ua1","ua2","ua3","ua4","ua5");
		assertInvisibleAdministrativeUnitsByFilter("section_manager_4", null, null, "ua3","ua4","ua5");
		
		assertInvisibleAdministrativeUnitsByFilter("usb_manager_1", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("usb_manager_2", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("usb_manager_3", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		
		assertInvisibleAdministrativeUnitsByFilter("activity_manager_1", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("activity_manager_2", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("activity_manager_3", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		
		assertInvisibleAdministrativeUnitsByFilter("ua_manager_1", null, null,"ua2","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("ua_manager_2", null, null,"ua1","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertInvisibleAdministrativeUnitsByFilter("ua_manager_3", null, null,"ua1","ua2","ua3","ua4","ua5","ua7");
	}
	
	@Test
	public void readVisibleAdministrativeUnitsWhereFilter(){
		assertVisibleAdministrativeUnitsByFilter("inconnu", null, null);
		assertVisibleAdministrativeUnitsByFilter("inconnu", "a", null);
		assertVisibleAdministrativeUnitsByFilter("inconnu", "a1", null);
		assertVisibleAdministrativeUnitsByFilter("inconnu", "2", null);
		
		assertVisibleAdministrativeUnitsByFilter("admin", null, null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertVisibleAdministrativeUnitsByFilter("admin", "a", null,"ua1","ua2","ua3","ua4","ua5","ua6","ua7","ua8","ua9");
		assertVisibleAdministrativeUnitsByFilter("admin", "a1", null,"ua1");
		assertVisibleAdministrativeUnitsByFilter("admin", "2", null,"ua2");
		
		assertVisibleAdministrativeUnitsByFilter("section_manager_1", null, null, "ua1","ua2");
		assertVisibleAdministrativeUnitsByFilter("section_manager_2", null, null, "ua3","ua4","ua5");
		assertVisibleAdministrativeUnitsByFilter("section_manager_3", null, null, "ua6","ua7","ua8","ua9");
		assertVisibleAdministrativeUnitsByFilter("section_manager_4", null, null, "ua1","ua2","ua6","ua7","ua8","ua9");
		
		assertVisibleAdministrativeUnitsByFilter("usb_manager_1", null, null);
		assertVisibleAdministrativeUnitsByFilter("usb_manager_2", null, null);
		assertVisibleAdministrativeUnitsByFilter("usb_manager_3", null, null);
		
		assertVisibleAdministrativeUnitsByFilter("activity_manager_1", null, null);
		assertVisibleAdministrativeUnitsByFilter("activity_manager_2", null, null);
		assertVisibleAdministrativeUnitsByFilter("activity_manager_3", null, null);
		
		assertVisibleAdministrativeUnitsByFilter("ua_manager_1", null, null,"ua1","ua3");
		assertVisibleAdministrativeUnitsByFilter("ua_manager_2", null, null,"ua2");
		assertVisibleAdministrativeUnitsByFilter("ua_manager_3", null, null,"ua6","ua8","ua9");
	}
	
	/**/

	private void assertInvisibleSectionsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeSectionQuerier.getInstance().readInvisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertVisibleSectionsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeSectionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertInvisibleBudgetSpecializationUnitsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readInvisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeBudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER)
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertVisibleBudgetSpecializationUnitsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readVisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertInvisibleActionsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeActionQuerier.getInstance().readInvisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertVisibleActionsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeActionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertInvisibleActivitiesByFilter(String actorCode,String code,String name,String...expectedCodes) {
		Collection<Scope> scopes = ScopeOfTypeActivityQuerier.getInstance().readInvisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name));
		assertScopesExactly(scopes,expectedCodes);
	}
	
	private void assertVisibleActivitiesByFilter(String actorCode,String code,String name,String...expectedCodes) {
		Collection<Scope> scopes = ScopeOfTypeActivityQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name));
		assertScopesExactly(scopes,expectedCodes);
	}
	
	private void assertInvisibleAdministrativeUnitsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readInvisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER)
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertVisibleAdministrativeUnitsByFilter(String actorCode,String code,String name,String...expectedCodes) {
		assertScopesExactly(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readInvisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER)
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode
						,ScopeQuerier.PARAMETER_NAME_CODE, code,ScopeQuerier.PARAMETER_NAME_NAME, name)),expectedCodes);
	}
	
	private void assertScopesExactly(Collection<Scope> scopes,String...expectedCodes) {
		assertScopes(scopes, Boolean.TRUE, expectedCodes);
	}
	
	private void assertScopes(Collection<Scope> scopes,Boolean exactly,String...expectedCodes) {
		if(CollectionHelper.isEmpty(scopes)) {
			assertThat(ArrayHelper.isEmpty(expectedCodes)).as("No scopes found").isTrue();
		}else {
			if(exactly == null || Boolean.TRUE.equals(exactly))
				assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
			else
				assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).contains(expectedCodes);
		}
	}
	
	/**/
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(
				new ScopeType().setCode(ScopeType.CODE_SECTION).setOrderNumber((byte)1)
				,new ScopeType().setCode(ScopeType.CODE_USB).setOrderNumber((byte)2)
				,new ScopeType().setCode(ScopeType.CODE_ACTION).setOrderNumber((byte)3)
				,new ScopeType().setCode(ScopeType.CODE_ACTIVITE).setOrderNumber((byte)4)
				,new ScopeType().setCode(ScopeType.CODE_IMPUTATION).setOrderNumber((byte)5)
				,new ScopeType().setCode(ScopeType.CODE_UA).setOrderNumber((byte)6));
		
		//Sections
		createSection("s1");
		createSection("s2");
		createSection("s3");
		
		//USBs
		createBudgetSpecializationUnit("usb1", "s1");
		createBudgetSpecializationUnit("usb2", "s1");
		createBudgetSpecializationUnit("usb3", "s1");		
		createBudgetSpecializationUnit("usb4", "s3");
		createBudgetSpecializationUnit("usb5", "s3");
		
		//Actions
		createAction("action1", "usb1");
		createAction("action2", "usb1");
		createAction("action3", "usb2");
		createAction("action4", "usb5");
		
		//Activities
		createActivity("a1", "action1");
		createActivity("a2", "action2");
		createActivity("a3", "action3");
		createActivity("a4", "action3");
		createActivity("a5", "action3");
		createActivity("a6", "action4");
		
		//Imputations
		createImputation("i1", "a1");
		
		//UAs
		createAdministrativeUnit("ua1", "s1");
		createAdministrativeUnit("ua2", "s1");
		createAdministrativeUnit("ua3", "s2");
		createAdministrativeUnit("ua4", "s2");
		createAdministrativeUnit("ua5", "s2");
		createAdministrativeUnit("ua6", "s3");
		createAdministrativeUnit("ua7", "s3");
		createAdministrativeUnit("ua8", "s3");
		createAdministrativeUnit("ua9", "s3");
		
		//Actors
		createActor("inconnu");
		createActor("admin","s1","s2","s3");
		
		createActor("section_manager_1","s1");
		createActor("section_manager_2","s2");
		createActor("section_manager_3","s3");
		createActor("section_manager_4","s1","s3");
		
		createActor("usb_manager_1","usb1");
		createActor("usb_manager_2","usb2");
		createActor("usb_manager_3","usb3","usb4","usb5");
		
		createActor("action_manager_1","action1");
		createActor("action_manager_2","action2","action3");
		createActor("action_manager_3","action4");
		
		createActor("activity_manager_1","a1");
		createActor("activity_manager_2","a2","a3","a4");
		createActor("activity_manager_3","a5","a6");
		
		createActor("ua_manager_1","ua1","ua3");
		createActor("ua_manager_2","ua2");
		createActor("ua_manager_3","ua6","ua8","ua9");
	}
	
	private void createSection(String code) {
		Scope sectionScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_SECTION);
		EntityCreator.getInstance().createManyInTransaction(sectionScope);
		Section section = new Section().setCode(sectionScope.getIdentifier());
		EntityCreator.getInstance().createManyInTransaction(section);
	}
	
	private void createBudgetSpecializationUnit(String code,String sectionIdentifier) {
		Scope usbScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_USB);
		BudgetSpecializationUnit usb = new BudgetSpecializationUnit().setCode(usbScope.getCode()).setSectionFromIdentifier(sectionIdentifier); 
		EntityCreator.getInstance().createManyInTransaction(usbScope,usb);
	}
	
	private void createAction(String code,String budgetSpecializationUnitFromIdentifierIdentifier) {
		Scope actionScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_ACTION);
		Action action = new Action().setIdentifier(actionScope.getIdentifier()).setCode(actionScope.getCode())
				.setBudgetSpecializationUnitFromIdentifier(budgetSpecializationUnitFromIdentifierIdentifier);
		action.setSection(action.getBudgetSpecializationUnit().getSection());
		EntityCreator.getInstance().createManyInTransaction(actionScope,action);	
	}
	
	private void createActivity(String code,String actionIdentifier) {
		Scope activityScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_ACTIVITE);
		Activity activity = new Activity().setIdentifier(activityScope.getIdentifier()).setCode(activityScope.getCode())
				.setActionFromIdentifier(actionIdentifier);
		activity.setBudgetSpecializationUnit(activity.getAction().getBudgetSpecializationUnit());
		activity.setSection(activity.getBudgetSpecializationUnit().getSection());
		EntityCreator.getInstance().createManyInTransaction(activityScope,activity);	
	}
	
	private void createImputation(String code,String activityIdentifier) {
		Scope imputationScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_IMPUTATION);
		Imputation imputation = new Imputation().setCode(code).setCode(code).setActivityFromIdentifier(activityIdentifier);
		imputation.setAction(imputation.getActivity().getAction());
		imputation.setBudgetSpecializationUnit(imputation.getActivity().getBudgetSpecializationUnit());
		imputation.setSection(imputation.getBudgetSpecializationUnit().getSection());
		EntityCreator.getInstance().createManyInTransaction(imputationScope,imputation);
	}
	
	private void createAdministrativeUnit(String code,String sectionIdentifier) {
		Scope administrativeUnitScope = new Scope().setCode(code).setTypeFromIdentifier(ScopeType.CODE_UA);
		AdministrativeUnit administrativeUnit = new AdministrativeUnit().setIdentifier(administrativeUnitScope.getIdentifier())
				.setCode(administrativeUnitScope.getCode()).setSectionFromIdentifier(sectionIdentifier);
		EntityCreator.getInstance().createManyInTransaction(administrativeUnitScope,administrativeUnit);
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