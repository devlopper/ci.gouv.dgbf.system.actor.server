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
		static String visible(String variableName) {
			return String.format(VISIBLE_FORMAT, variableName);
		}
		
		static String visible() {
			return visible(ACTOR_SCOPE_VARIABLE_NAME);
		}
		
		static String visibleBy(String parameterNameActorCode,String variableName) {
			return and(StringHelper.isBlank(parameterNameActorCode) ? null : variableName+".actor.code = "+parameterNameActorCode
					,variableName+".scope = t",visible(variableName));
		}
		
		static String visibleBy(String parameterNameActorCode) {
			return visibleBy(parameterNameActorCode, ACTOR_SCOPE_VARIABLE_NAME);
		}
		
		static String selfVisible(String parameterNameActorCode) {
			return exists(select("v.identifier"),from("ActorScope v"),where(visibleBy(parameterNameActorCode)));
		}
		
		static String selfVisible() {
			return selfVisible(":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		/* It has a visible child */
		
		static String childVisible(String tupleName,String variableName,String fieldName,String parameterNameActorCode) {
			return 
				exists(
					select("actorScope.identifier")
					,from("ActorScope actorScope")
					,"JOIN Scope scopeChild ON actorScope.scope = scopeChild"
					,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = scopeChild"
					,where(and(variableName+"."+fieldName+" = t",StringHelper.isBlank(parameterNameActorCode) ? null : "actorScope.actor.code = "+parameterNameActorCode))
				);
		}
		
		static String childVisible(String tupleName,String variableName,String fieldName) {
			return childVisible(tupleName, variableName, fieldName, ":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		static String childVisible(Class<?> klass,String fieldName,String parameterNameActorCode) {
			return childVisible(klass.getSimpleName(), StringHelper.getVariableNameFrom(klass.getSimpleName()), fieldName,parameterNameActorCode);
		}
		
		static String childVisible(Class<?> klass,String fieldName) {
			return childVisible(klass, fieldName,":"+PARAMETER_NAME_ACTOR_CODE);
		}
		
		/* It has a visible parent */
		
		static String parentVisible(String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName,String parameterNameActorCode) {
			String p1 = jpql(
					select("actorScope.identifier")
					,from("ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent")
					,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = scopeParent"
					,StringHelper.isBlank(parameterNameActorCode) ? null : where("actorScope.actor.code = "+parameterNameActorCode)
			);
			
			String p2 = exists(
					select(variableName)
					,from(tupleName+" "+variableName)
					,where(and(
							variableName+" = t",variableName+"."+fieldName+" = "+fieldName
							,not(
									exists(select("actorScope"+tupleName+" ")+from("ActorScope actorScope"+tupleName+" ")
										+ where(and("actorScope"+tupleName+".scope = t"
												,StringHelper.isBlank(parameterNameActorCode) ? null : "actorScope"+tupleName+".actor.code = "+parameterNameActorCode
												,"actorScope"+tupleName+".visible IS NOT NULL","actorScope"+tupleName+".visible = false"
												))
										)
								)
					))
			);
			return exists(and(p1,p2));
		}
		
		static String parentVisible(String parentTupleName,String tupleName,String parameterNameActorCode) {
			String variableName = StringHelper.getVariableNameFrom(parentTupleName);
			return parentVisible(parentTupleName, variableName, tupleName, StringHelper.getVariableNameFrom(tupleName), variableName,parameterNameActorCode);
		}
		
		static String parentVisible(String parentTupleName,String tupleName) {
			return parentVisible(parentTupleName, tupleName, ":"+PARAMETER_NAME_ACTOR_CODE);
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
						selfVisible(parameterNameActorCode)
						,childVisible(AdministrativeUnit.class,"section",parameterNameActorCode)
						,childVisible(BudgetSpecializationUnit.class,"section",parameterNameActorCode)
						,childVisible(Action.class,"section",parameterNameActorCode)
						,childVisible(Activity.class,"section",parameterNameActorCode)
						,childVisible(Imputation.class,"section",parameterNameActorCode)
					))
				));
		}
		
		/* Administrative Unit */
		
		static String hasVisibleAdministrativeUnit(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_UA),
					parenthesis(or(
						selfVisible(parameterNameActorCode)
						,parentVisible("Section", "AdministrativeUnit",parameterNameActorCode)
					))
				));
		}
		
		/* Budget Specialization Unit */
		
		static String hasVisibleBudgetSpecializationUnit(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_USB),	
					parenthesis(or(
						selfVisible(parameterNameActorCode)
						,parentVisible("Section", "BudgetSpecializationUnit",parameterNameActorCode)
						,childVisible(Action.class,"budgetSpecializationUnit",parameterNameActorCode)
						,childVisible(Activity.class,"budgetSpecializationUnit",parameterNameActorCode)
						,childVisible(Imputation.class,"budgetSpecializationUnit",parameterNameActorCode)
					))
				));
		}
		
		/* Activity */
		
		static String hasVisibleAction(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTION),	
					parenthesis(or(
						selfVisible(parameterNameActorCode)
						,parentVisible("Section", "Action",parameterNameActorCode)
						,parentVisible("BudgetSpecializationUnit", "Action",parameterNameActorCode)
						,childVisible(Activity.class,"action",parameterNameActorCode)
						,childVisible(Imputation.class,"action",parameterNameActorCode)
					))
				));
		}
		
		/* Activity */
		
		static String hasVisibleActivity(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTIVITE),	
					parenthesis(or(
						selfVisible(parameterNameActorCode)
						,parentVisible("Section", "Activity",parameterNameActorCode)
						,parentVisible("AdministrativeUnit", "Activity",parameterNameActorCode)
						,parentVisible("BudgetSpecializationUnit", "Activity",parameterNameActorCode)
						,parentVisible("Action", "Activity",parameterNameActorCode)
						,childVisible(Imputation.class,"activity",parameterNameActorCode)
					))
				));
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