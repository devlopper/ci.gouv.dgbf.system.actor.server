package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.exists;
import static org.cyk.utility.persistence.query.Language.Where.not;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.Language;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
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
			String selectFrom = jpql(
					select("p.identifier")
					,from("ActorScope p JOIN Scope s ON p.scope = s")
					,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = s"
			);
			String p1 = StringHelper.isBlank(parameterNameActorCode) ? null : "p.actor.code = "+parameterNameActorCode;
			String p2 = exists(
					select(variableName)
					,from(tupleName+" "+variableName)
					,where(and(
							variableName+" = t",variableName+"."+fieldName+" = "+fieldName
							,not(
									exists(select("p"+tupleName+" ")+from("ActorScope p"+tupleName+" ")
										+ where(and("p"+tupleName+".scope = t"
												,StringHelper.isBlank(parameterNameActorCode) ? null : "p"+tupleName+".actor.code = "+parameterNameActorCode
												,"p"+tupleName+".visible IS NOT NULL","p"+tupleName+".visible = false"
												))
										)
								)
					))
			);
			return jpql(exists(selectFrom+" WHERE "+(p1 == null ? "" : p1+" AND ")+p2));
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
		
		String TYPE_CODE_EQUALS = "t.type.code = :typeCode";
		static String typeCodeEquals() {
			return TYPE_CODE_EQUALS;
		}
		
		static String scopeVisible(String typeCode,String parameterNameActorCode,Boolean negate) {
			String string = null;
			if(StringHelper.isBlank(typeCode))
				string = parenthesis(or(
						hasVisibleSection(parameterNameActorCode),hasVisibleAdministrativeUnit(parameterNameActorCode)
						,hasVisibleBudgetSpecializationUnit(parameterNameActorCode),hasVisibleAction(parameterNameActorCode),hasVisibleActivity(parameterNameActorCode)
						,hasVisibleBudgetCategory(parameterNameActorCode)
					));
			else if(ScopeType.CODE_SECTION.equals(typeCode))
				string = hasVisibleSection(parameterNameActorCode);
			else if(ScopeType.CODE_UA.equals(typeCode))
				string = hasVisibleAdministrativeUnit(parameterNameActorCode);
			else if(ScopeType.CODE_USB.equals(typeCode))
				string = hasVisibleBudgetSpecializationUnit(parameterNameActorCode);
			else if(ScopeType.CODE_ACTION.equals(typeCode))
				string = hasVisibleAction(parameterNameActorCode);
			else if(ScopeType.CODE_ACTIVITE.equals(typeCode))
				string = hasVisibleActivity(parameterNameActorCode);
			else if(ScopeType.CODE_CATEGORIE_BUDGET.equals(typeCode))
				string = hasVisibleBudgetCategory(parameterNameActorCode);
			
			if(Boolean.TRUE.equals(negate))
				string = Language.Where.not(string);
			
			if(StringHelper.isBlank(string))
				throw new RuntimeException(String.format("Visible predicate of scope type <<%s>> not yet implemented", typeCode));
			return string;
		}
		
		static String hasVisibleBudgetCategory(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_CATEGORIE_BUDGET),				
					parenthesis(or(
						selfVisible(parameterNameActorCode)
					))
				));
		}
		
		static String hasVisibleActivityCategory(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_CATEGORIE_ACTIVITE),				
					parenthesis(or(
						selfVisible(parameterNameActorCode)
						,childVisible(Activity.class,"activityCategory",parameterNameActorCode)
						,childVisible(Imputation.class,"activityCategory",parameterNameActorCode)
					))
				));
		}
		
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
		
		static String hasVisibleAdministrativeUnit(String parameterNameActorCode) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_UA),
					parenthesis(or(
						selfVisible(parameterNameActorCode)
						,parentVisible("Section", "AdministrativeUnit",parameterNameActorCode)
					))
				));
		}
		
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
	}
}