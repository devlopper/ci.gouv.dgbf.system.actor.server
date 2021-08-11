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

import java.io.Serializable;

import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public abstract class AbstractVisibilityQueryStringBuilder implements Serializable {

	public String actor(Boolean parameterable,String variableName,Boolean joinable) {
		return Boolean.TRUE.equals(parameterable) 
				? variableName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE 
				: (Boolean.TRUE.equals(joinable) ? variableName+".actor = "+PARENT_VARIABLE_NAME : null);
	}
	
	private static final String IS_VISIBLE_FORMAT = "(%1$s.visible IS NULL OR %1$s.visible = true)";
	private static final String IS_NOT_VISIBLE_FORMAT = "%1$s.visible IS NOT NULL AND %1$s.visible = false";
	static String visible(String fieldName,Boolean negate) {
		return String.format(Boolean.TRUE.equals(negate) ? IS_NOT_VISIBLE_FORMAT : IS_VISIBLE_FORMAT, fieldName);
	}
	
	public String visible() {
		return visible(ACTOR_SCOPE_VARIABLE_NAME,null);
	}
	
	public String visibleBy(Boolean actorable,Boolean parameterable,String parentFieldName) {
		return and(
			/* Join */FieldHelper.join(ACTOR_SCOPE_VARIABLE_NAME,parentFieldName)+" = t"
			/* Filter by actor code*/,actor(actorable, ACTOR_SCOPE_VARIABLE_NAME,null) 
			,visible()
		);
	}
	
	public String selfVisible(Boolean actorable,Boolean parameterable,String parentFieldName) {
		return exists(select("v.identifier"),from("ActorScope v"),where(visibleBy(actorable,parameterable,parentFieldName)));
	}
	
	/* It has a visible child */
	
	public String childVisible(Boolean actorable,Boolean parameterable,Class<?> klass,String parentFieldName) {
		String tupleName = klass.getSimpleName();
		String variableName = StringHelper.getVariableNameFrom(tupleName);
		return 
				exists(
					select("_as.identifier")
					,from("ActorScope _as")
					,"JOIN Scope child ON _as.scope = child"
					,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = child"
					,where(and(
							/* Join */variableName+"."+parentFieldName+" = t"
							,/* Filter by actor code */Boolean.TRUE.equals(actorable) ? actor(parameterable, "_as", Boolean.TRUE) : null
							))
				);
	}
	
	public String childVisible(Boolean actorable,Boolean parameterable,Class<?> klass,Class<?> parent) {
		return childVisible(actorable,parameterable, klass, StringHelper.getVariableNameFrom(parent.getSimpleName()));
	}
	
	/* It has a visible parent */
	
	public String parentVisible(Boolean actorable,Boolean parameterable,String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName) {
		String selectFrom = jpql(
				select("p.identifier")
				,from("ActorScope p JOIN Scope s ON p.scope = s")
				,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = s"
		);
		String p1 = Boolean.TRUE.equals(actorable) ? actor(parameterable, "p", null) : null;
		String p2 = exists(
				select(variableName)
				,from(tupleName+" "+variableName)
				,where(and(
						variableName+" = t",variableName+"."+fieldName+" = "+fieldName
						,not(
								exists(select("p"+tupleName+" ")+from("ActorScope p"+tupleName+" ")
									+ where(and("p"+tupleName+".scope = t"
											,Boolean.TRUE.equals(actorable) ? actor(parameterable, "p"+tupleName, null) : null
											,visible("p"+tupleName, Boolean.TRUE) //"p"+tupleName+".visible IS NOT NULL","p"+tupleName+".visible = false"
											))
									)
							)
				))
		);
		return jpql(exists(selectFrom+" WHERE "+(p1 == null ? ConstantEmpty.STRING : p1+" AND ")+p2));
	}
	
	public String parentVisible(Boolean actorable,Boolean parameterable,Class<?> klass,Class<?> child) {
		String parentTupleName = klass.getSimpleName();
		String variableName = StringHelper.getVariableNameFrom(parentTupleName);
		String childTupleName = child.getSimpleName();
		return parentVisible(actorable,parameterable,parentTupleName, variableName, childTupleName, StringHelper.getVariableNameFrom(childTupleName), variableName);
	}
	
	private static final String IS_TYPE_CODE_FORMAT = "t.type.code = '%s'";
	public String isTypeCode(String code) {
		return String.format(IS_TYPE_CODE_FORMAT, code);
	}
	
	private static final String TYPE_CODE_EQUALS = "t.type.code = :typeCode";
	public String typeCodeEquals() {
		return TYPE_CODE_EQUALS;
	}
	
	public String scopeVisible(String typeCode,Boolean actorable,Boolean parameterable,Boolean negate) {
		String string = null;
		if(StringHelper.isBlank(typeCode))
			string = parenthesis(or(
					hasVisibleSection(actorable,parameterable,negate),hasVisibleAdministrativeUnit(actorable,parameterable,negate)
					,hasVisibleBudgetSpecializationUnit(actorable,parameterable,negate),hasVisibleAction(actorable,parameterable,negate)
					,hasVisibleActivity(actorable,parameterable,negate),hasVisibleBudgetCategory(actorable,parameterable,negate)
				));
		else if(ScopeType.CODE_SECTION.equals(typeCode))
			string = hasVisibleSection(actorable,parameterable,negate);
		else if(ScopeType.CODE_UA.equals(typeCode))
			string = hasVisibleAdministrativeUnit(actorable,parameterable,negate);
		else if(ScopeType.CODE_USB.equals(typeCode))
			string = hasVisibleBudgetSpecializationUnit(actorable,parameterable,negate);
		else if(ScopeType.CODE_ACTION.equals(typeCode))
			string = hasVisibleAction(actorable,parameterable,negate);
		else if(ScopeType.CODE_ACTIVITE.equals(typeCode))
			string = hasVisibleActivity(actorable,parameterable,negate);
		else if(ScopeType.CODE_CATEGORIE_BUDGET.equals(typeCode))
			string = hasVisibleBudgetCategory(actorable,parameterable,negate);
		
		if(StringHelper.isBlank(string))
			throw new RuntimeException(String.format("Visible predicate of scope type <<%s>> not yet implemented", typeCode));
		return string;
	}
	
	public String hasVisibleBudgetCategory(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_CATEGORIE_BUDGET),				
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
				)),negate)
			));
	}
	
	public String hasVisibleActivityCategory(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_CATEGORIE_ACTIVITE),				
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
					,childVisible(actorable,parameterable,Activity.class,"activityCategory")
					,childVisible(actorable,parameterable,Imputation.class,"activityCategory")
				)),negate)
			));
	}
	
	public String hasVisibleSection(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_SECTION),				
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
					,childVisible(actorable,parameterable,AdministrativeUnit.class,Section.class)
					,childVisible(actorable,parameterable,BudgetSpecializationUnit.class,Section.class)
					,childVisible(actorable,parameterable,Action.class,Section.class)
					,childVisible(actorable,parameterable,Activity.class,Section.class)
					,childVisible(actorable,parameterable,Imputation.class,Section.class)
				)),negate)
			));
	}
	
	public String hasVisibleAdministrativeUnit(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_UA),
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
					,parentVisible(actorable,parameterable,Section.class, AdministrativeUnit.class)
				)),negate)
			));
	}
	
	public String hasVisibleBudgetSpecializationUnit(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_USB),	
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
					,parentVisible(actorable,parameterable,Section.class, BudgetSpecializationUnit.class)
					,childVisible(actorable,parameterable,Action.class,BudgetSpecializationUnit.class)
					,childVisible(actorable,parameterable,Activity.class,BudgetSpecializationUnit.class)
					,childVisible(actorable,parameterable,Imputation.class,BudgetSpecializationUnit.class)
				)),negate)
			));
	}
	
	public String hasVisibleAction(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_ACTION),	
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
					,parentVisible(actorable,parameterable,Section.class, Action.class)
					,parentVisible(actorable,parameterable,BudgetSpecializationUnit.class, Action.class)
					,childVisible(actorable,parameterable,Activity.class,Action.class)
					,childVisible(actorable,parameterable,Imputation.class,Action.class)
				)),negate)
			));
	}
	
	public String hasVisibleActivity(Boolean actorable,Boolean parameterable,Boolean negate) {
		return parenthesis(and(
				isTypeCode(ScopeType.CODE_ACTIVITE),	
				notIfTrue(parenthesis(or(
					selfVisible(actorable,parameterable,"scope")
					,parentVisible(actorable,parameterable,Section.class, Activity.class)
					,parentVisible(actorable,parameterable,AdministrativeUnit.class, Activity.class)
					,parentVisible(actorable,parameterable,BudgetSpecializationUnit.class, Activity.class)
					,parentVisible(actorable,parameterable,Action.class, Activity.class)
					,childVisible(actorable,parameterable,Imputation.class,Activity.class)
				)),negate)
			));
	}
	
	public static final String PARENT_VARIABLE_NAME = "t";
	public static final String ACTOR_SCOPE_VARIABLE_NAME = "v";
	public static final String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	public static final String PARAMETER_NAME_SCOPE_IDENTIFIER = "scopeIdentifier";	
}