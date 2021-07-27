package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.exists;
import static org.cyk.utility.persistence.query.Language.Where.not;
import static org.cyk.utility.persistence.query.Language.Where.notIfTrue;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import org.cyk.utility.__kernel__.string.StringHelper;

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
		
		static String visibleBy(Boolean actorable,String variableName) {
			return and(actorable == null || !actorable ? null : variableName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE
					,variableName+".scope = t",visible(variableName));
		}
		
		static String visibleBy(Boolean actorable) {
			return visibleBy(actorable, ACTOR_SCOPE_VARIABLE_NAME);
		}
		
		static String selfVisible(Boolean actorable) {
			return exists(select("v.identifier"),from("ActorScope v"),where(visibleBy(actorable)));
		}
		
		static String selfVisible() {
			return selfVisible(Boolean.TRUE);
		}
		
		/* It has a visible child */
		
		static String childVisible(String tupleName,String variableName,String fieldName,Boolean actorable) {
			return 
				exists(
					select("actorScope.identifier")
					,from("ActorScope actorScope")
					,"JOIN Scope scopeChild ON actorScope.scope = scopeChild"
					,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = scopeChild"
					,where(and(variableName+"."+fieldName+" = t",actorable == null || !actorable ? null : "actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE))
				);
		}
		
		static String childVisible(String tupleName,String variableName,String fieldName) {
			return childVisible(tupleName, variableName, fieldName, Boolean.TRUE);
		}
		
		static String childVisible(Class<?> klass,String fieldName,Boolean actorable) {
			return childVisible(klass.getSimpleName(), StringHelper.getVariableNameFrom(klass.getSimpleName()), fieldName,actorable);
		}
		
		static String childVisible(Class<?> klass,String fieldName) {
			return childVisible(klass, fieldName,Boolean.TRUE);
		}
		
		/* It has a visible parent */
		
		static String parentVisible(String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName,Boolean actorable) {
			String selectFrom = jpql(
					select("p.identifier")
					,from("ActorScope p JOIN Scope s ON p.scope = s")
					,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = s"
			);
			String p1 = actorable == null || !actorable ? null : "p.actor.code = :"+PARAMETER_NAME_ACTOR_CODE;
			String p2 = exists(
					select(variableName)
					,from(tupleName+" "+variableName)
					,where(and(
							variableName+" = t",variableName+"."+fieldName+" = "+fieldName
							,not(
									exists(select("p"+tupleName+" ")+from("ActorScope p"+tupleName+" ")
										+ where(and("p"+tupleName+".scope = t"
												,actorable == null || !actorable ? null : "p"+tupleName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE
												,"p"+tupleName+".visible IS NOT NULL","p"+tupleName+".visible = false"
												))
										)
								)
					))
			);
			return jpql(exists(selectFrom+" WHERE "+(p1 == null ? "" : p1+" AND ")+p2));
		}
		
		static String parentVisible(String parentTupleName,String tupleName,Boolean actorable) {
			String variableName = StringHelper.getVariableNameFrom(parentTupleName);
			return parentVisible(parentTupleName, variableName, tupleName, StringHelper.getVariableNameFrom(tupleName), variableName,actorable);
		}
		
		static String parentVisible(String parentTupleName,String tupleName) {
			return parentVisible(parentTupleName, tupleName, Boolean.TRUE);
		}
		
		String IS_TYPE_CODE_FORMAT = "t.type.code = '%s'";
		static String isTypeCode(String code) {
			return String.format(IS_TYPE_CODE_FORMAT, code);
		}
		
		String TYPE_CODE_EQUALS = "t.type.code = :typeCode";
		static String typeCodeEquals() {
			return TYPE_CODE_EQUALS;
		}
		
		static String scopeVisible(String typeCode,Boolean actorable,Boolean negate) {
			String string = null;
			if(StringHelper.isBlank(typeCode))
				string = parenthesis(or(
						hasVisibleSection(actorable,negate),hasVisibleAdministrativeUnit(actorable,negate)
						,hasVisibleBudgetSpecializationUnit(actorable,negate),hasVisibleAction(actorable,negate),hasVisibleActivity(actorable,negate)
						,hasVisibleBudgetCategory(actorable,negate)
					));
			else if(ScopeType.CODE_SECTION.equals(typeCode))
				string = hasVisibleSection(actorable,negate);
			else if(ScopeType.CODE_UA.equals(typeCode))
				string = hasVisibleAdministrativeUnit(actorable,negate);
			else if(ScopeType.CODE_USB.equals(typeCode))
				string = hasVisibleBudgetSpecializationUnit(actorable,negate);
			else if(ScopeType.CODE_ACTION.equals(typeCode))
				string = hasVisibleAction(actorable,negate);
			else if(ScopeType.CODE_ACTIVITE.equals(typeCode))
				string = hasVisibleActivity(actorable,negate);
			else if(ScopeType.CODE_CATEGORIE_BUDGET.equals(typeCode))
				string = hasVisibleBudgetCategory(actorable,negate);
			
			//if(Boolean.TRUE.equals(negate))
			//	string = Language.Where.not(string);
			
			if(StringHelper.isBlank(string))
				throw new RuntimeException(String.format("Visible predicate of scope type <<%s>> not yet implemented", typeCode));
			return string;
		}
		
		static String hasVisibleBudgetCategory(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_CATEGORIE_BUDGET),				
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
					)),negate)
				));
		}
		
		static String hasVisibleActivityCategory(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_CATEGORIE_ACTIVITE),				
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
						,childVisible(Activity.class,"activityCategory",actorable)
						,childVisible(Imputation.class,"activityCategory",actorable)
					)),negate)
				));
		}
		
		static String hasVisibleSection(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_SECTION),				
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
						,childVisible(AdministrativeUnit.class,"section",actorable)
						,childVisible(BudgetSpecializationUnit.class,"section",actorable)
						,childVisible(Action.class,"section",actorable)
						,childVisible(Activity.class,"section",actorable)
						,childVisible(Imputation.class,"section",actorable)
					)),negate)
				));
		}
		
		static String hasVisibleAdministrativeUnit(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_UA),
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
						,parentVisible("Section", "AdministrativeUnit",actorable)
					)),negate)
				));
		}
		
		static String hasVisibleBudgetSpecializationUnit(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_USB),	
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
						,parentVisible("Section", "BudgetSpecializationUnit",actorable)
						,childVisible(Action.class,"budgetSpecializationUnit",actorable)
						,childVisible(Activity.class,"budgetSpecializationUnit",actorable)
						,childVisible(Imputation.class,"budgetSpecializationUnit",actorable)
					)),negate)
				));
		}
		
		static String hasVisibleAction(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTION),	
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
						,parentVisible("Section", "Action",actorable)
						,parentVisible("BudgetSpecializationUnit", "Action",actorable)
						,childVisible(Activity.class,"action",actorable)
						,childVisible(Imputation.class,"action",actorable)
					)),negate)
				));
		}
		
		static String hasVisibleActivity(Boolean actorable,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTIVITE),	
					notIfTrue(parenthesis(or(
						selfVisible(actorable)
						,parentVisible("Section", "Activity",actorable)
						,parentVisible("AdministrativeUnit", "Activity",actorable)
						,parentVisible("BudgetSpecializationUnit", "Activity",actorable)
						,parentVisible("Action", "Activity",actorable)
						,childVisible(Imputation.class,"activity",actorable)
					)),negate)
				));
		}
	}
}