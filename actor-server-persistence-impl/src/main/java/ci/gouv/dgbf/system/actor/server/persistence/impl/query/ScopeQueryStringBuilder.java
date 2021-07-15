package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.exists;
import static org.cyk.utility.persistence.query.Language.Where.like;
import static org.cyk.utility.persistence.query.Language.Where.not;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public interface ScopeQueryStringBuilder {
	
	public static interface Predicate {
		
		String ACTOR_SCOPE_VARIABLE_NAME = "v";
		String PARAMETER_NAME_ACTOR_CODE = "actorCode";
		
		String VISIBLE_FORMAT = "(%1$s.visible IS NULL OR %1$s.visible = true)";
		static String isVisible(String variableName) {
			return String.format(VISIBLE_FORMAT, variableName);
		}
		
		static String isVisible() {
			return isVisible(ACTOR_SCOPE_VARIABLE_NAME);
		}
		
		String ACTOR_SCOPE_VISIBLE = "%1$s.actor.code = %2$s AND %1$s.scope = t AND %3$s";
		static String isVisibleByActor(String parameterNameActorCode,String variableName) {
			return String.format(ACTOR_SCOPE_VISIBLE, variableName,parameterNameActorCode,isVisible(variableName));
		}
		
		static String isVisibleByActor(String parameterNameActorCode) {
			return isVisibleByActor(parameterNameActorCode, ACTOR_SCOPE_VARIABLE_NAME);
		}
		
		static String hasVisibility(String parameterNameActorCode) {
			return exists(select("v.identifier"),from("ActorScope v"),where(isVisibleByActor(parameterNameActorCode)));
		}
		
		static String hasVisibility() {
			return hasVisibility(":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		/* It has a visible child */
		
		static String hasVisibleChild(String tupleName,String variableName,String fieldName,String parameterNameActorCode) {
			return 
				exists(
					select("actorScope.identifier")
					,from("ActorScope actorScope")
					,"JOIN Scope scopeChild ON actorScope.scope = scopeChild"
					,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = scopeChild"
					,where(and(variableName+"."+fieldName+" = t","actorScope.actor.code = "+parameterNameActorCode))
				);
		}
		
		static String hasVisibleChild(String tupleName,String variableName,String fieldName) {
			return hasVisibleChild(tupleName, variableName, fieldName, ":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		static String hasVisibleChild(Class<?> klass,String fieldName,String parameterNameActorCode) {
			return hasVisibleChild(klass.getSimpleName(), StringHelper.getVariableNameFrom(klass.getSimpleName()), fieldName,parameterNameActorCode);
		}
		
		static String hasVisibleChild(Class<?> klass,String fieldName) {
			return hasVisibleChild(klass, fieldName,":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		/* It has a visible parent */
		
		static String hasVisibleParent(String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName,String parameterNameActorCode) {
			return 
				exists(and(
					jpql(
							select("actorScope.identifier")
							,from("ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent")
							,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = scopeParent"
							,where("actorScope.actor.code = "+parameterNameActorCode)
					)
					,exists(
							select(variableName)
							,from(tupleName+" "+variableName)
							,where(and(
									variableName+" = t",variableName+"."+fieldName+" = "+fieldName
									,not(
											exists(select("actorScope"+tupleName+" ")+from("ActorScope actorScope"+tupleName+" ")
												+ where(and("actorScope"+tupleName+".scope = t"
														,"actorScope"+tupleName+".actor.code = "+parameterNameActorCode
														,"actorScope"+tupleName+".visible IS NOT NULL","actorScope"+tupleName+".visible = false"
														))
												)
										)
							))
					)
				));
		}
		
		static String hasVisibleParent(String parentTupleName,String tupleName,String parameterNameActorCode) {
			String variableName = StringHelper.getVariableNameFrom(parentTupleName);
			return hasVisibleParent(parentTupleName, variableName, tupleName, StringHelper.getVariableNameFrom(tupleName), variableName,parameterNameActorCode);
		}
		
		static String hasVisibleParent(String parentTupleName,String tupleName) {
			return hasVisibleParent(parentTupleName, tupleName, ":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		String IS_TYPE_CODE_FORMAT = "t.type.code = '%s'";
		static String isTypeCode(String code) {
			return String.format(IS_TYPE_CODE_FORMAT, code);
		}
		
		static String filter(String parameterNameCode,String parameterNameName) {
			return and(
				like("t", Scope.FIELD_CODE, parameterNameCode),
				like("t", Scope.FIELD_NAME, parameterNameName, ScopeQuerier.NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			);
		}
		
		/* Section */
		
		static String hasVisibleSection(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_SECTION),				
					parenthesis(or(
						hasVisibility(parameterNameActorCode)
						,hasVisibleChild(AdministrativeUnit.class,"section",parameterNameActorCode)
						,hasVisibleChild(BudgetSpecializationUnit.class,"section",parameterNameActorCode)
						,hasVisibleChild(Action.class,"section",parameterNameActorCode)
						,hasVisibleChild(Activity.class,"section",parameterNameActorCode)
						,hasVisibleChild(Imputation.class,"section",parameterNameActorCode)
					))
				));
		}
		
		static String hasVisibleSection(String parameterNameActorCode,String parameterNameCode,String parameterNameName) {
			return and(hasVisibleSection(parameterNameActorCode),filter(parameterNameCode, parameterNameName));
		}
		
		/* Administrative Unit */
		
		static String hasVisibleAdministrativeUnit(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_UA),
					parenthesis(or(
						hasVisibility(parameterNameActorCode),
						hasVisibleParent("Section", "AdministrativeUnit")
					))
				));
		}
		
		static String hasVisibleAdministrativeUnit(String parameterNameActorCode,String parameterNameCode,String parameterNameName) {
			return and(hasVisibleAdministrativeUnit(parameterNameActorCode),filter(parameterNameCode, parameterNameName));
		}
		
		/* Budget Specialization Unit */
		
		static String hasVisibleBudgetSpecializationUnit(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_USB),	
					parenthesis(or(
						hasVisibility(parameterNameActorCode),
						hasVisibleParent("Section", "AdministrativeUnit")
					))
				));
		}
		
		static String hasVisibleBudgetSpecializationUnit(String parameterNameActorCode,String parameterNameCode,String parameterNameName) {
			return and(hasVisibleBudgetSpecializationUnit(parameterNameActorCode),filter(parameterNameCode, parameterNameName));
		}
		
		/* Activity */
		
		static String hasVisibleAction(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTION),	
					parenthesis(or(
						hasVisibility(parameterNameActorCode),
						hasVisibleParent("Section", "Action"),
						hasVisibleParent("BudgetSpecializationUnit", "Action")
					))
				));
		}
		
		static String hasVisibleAction(String parameterNameActorCode,String parameterNameCode,String parameterNameName) {
			return and(hasVisibleAction(parameterNameActorCode),filter(parameterNameCode, parameterNameName));
		}
		
		/* Activity */
		
		static String hasVisibleActivity(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTIVITE),	
					parenthesis(or(
						hasVisibility(parameterNameActorCode),
						hasVisibleParent("Section", "Activity"),
						hasVisibleParent("BudgetSpecializationUnit", "Activity"),
						hasVisibleParent("Action", "Activity")
					))
				));
		}
		
		static String hasVisibleActivity(String parameterNameActorCode,String parameterNameCode,String parameterNameName) {
			return and(hasVisibleActivity(parameterNameActorCode),filter(parameterNameCode, parameterNameName));
		}
		
		/**/
		
		public static interface Arguments {
			static void addParentCodeNameContains(QueryExecutorArguments arguments,Filter filter,Collection<Class<?>> classes) {
				for(Class<?> klass : classes) {
					String variableName = StringHelper.getVariableNameFrom(klass.getSimpleName()+"CodeName");
					filter.addFieldsContains(arguments,variableName);
					filter.addFieldContainsStringOrWords(variableName, ScopeQuerier.NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);	
				}
			}
			
			static void addParentCodeNameContains(QueryExecutorArguments arguments,Filter filter,Class<?>...classes) {
				addParentCodeNameContains(arguments, filter, CollectionHelper.listOf(classes));
			}
		}
	}
}