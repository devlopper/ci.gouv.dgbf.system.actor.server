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

import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.persistence.PersistenceHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

public interface VisibilityQueryStringBuilder {
	
	@Getter @Setter @Accessors(chain=true)
	public static class PredicateBuilder {
		
		private Class<?> klass;
		private String PARENT_VARIABLE_NAME = "t";
		private String ACTOR_SCOPE_VARIABLE_NAME = "v";
		private String PARAMETER_NAME_ACTOR_CODE = "actorCode";
		private String PARAMETER_NAME_SCOPE_IDENTIFIER = "scopeIdentifier";
		
		/**
		 * Actor : actors having visibility on any or some scopes<br/>
		 * Scope : visible scopes by any or some actors
		 * @param klass
		 * @param variableName
		 * @param some
		 * @return
		 */
		public String join(Class<?> klass,Boolean some,String variableName,String fieldName) {
			String join = null;
			if(StringHelper.isBlank(fieldName))
				fieldName = variableName+"."+StringHelper.getVariableNameFrom(klass.getSimpleName());
			String natural = fieldName+" = "+PARENT_VARIABLE_NAME;
			if(Actor.class.equals(klass)) {
				if(Boolean.TRUE.equals(some))
					join = and(natural,variableName+".scope.identifier = :"+PARAMETER_NAME_SCOPE_IDENTIFIER);
				else
					join = natural;
			}else if(Scope.class.equals(klass)) {
				if(Boolean.TRUE.equals(some))
					join = and(natural,variableName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE);
				else
					join = natural;
			}
			return join;
		}
		
		public String actor(Boolean parameterable,String variableName,Boolean joinable) {
			return Boolean.TRUE.equals(parameterable) 
					? variableName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE 
					: (Boolean.TRUE.equals(joinable) ? variableName+".actor = "+PARENT_VARIABLE_NAME : null);
		}
		
		public String scope(Boolean parameterable,String variableName,Boolean joinable) {
			return Boolean.TRUE.equals(parameterable) 
					? variableName+".scope.identifier = :"+PARAMETER_NAME_SCOPE_IDENTIFIER 
					: (Boolean.TRUE.equals(joinable) ? variableName+".scope = "+PARENT_VARIABLE_NAME : null);
		}
		
		String IS_VISIBLE_FORMAT = "(%1$s.visible IS NULL OR %1$s.visible = true)";
		String IS_NOT_VISIBLE_FORMAT = "%1$s.visible IS NOT NULL AND %1$s.visible = false";
		public String visible(String fieldName,Boolean negate) {
			return String.format(Boolean.TRUE.equals(negate) ? IS_NOT_VISIBLE_FORMAT : IS_VISIBLE_FORMAT, fieldName);
		}
		
		public String visible() {
			return visible(ACTOR_SCOPE_VARIABLE_NAME, null);
		}
		
		public String visibleBy(Boolean actorable,Boolean parameterable,String parentFieldName) {
			return and(//join(Tuple.ACTOR, ACTOR_SCOPE_VARIABLE_NAME,parameterable),
				/* Join */FieldHelper.join(ACTOR_SCOPE_VARIABLE_NAME,parentFieldName)+" = t"
				/* Filter by actor code*/,actor(parameterable, ACTOR_SCOPE_VARIABLE_NAME,null) 
				,visible()
			);
		}
		
		public String selfVisible(Class<?> klass,Boolean some) {
			return exists(select("v.identifier"),from("ActorScope v"),where(and(join(klass,some, ACTOR_SCOPE_VARIABLE_NAME,null),visible())));
		}
		
		public String selfVisible(Boolean actorable,Boolean parameterable,String parentFieldName) {
			return exists(select("v.identifier"),from("ActorScope v"),where(visibleBy(actorable,parameterable,parentFieldName)));
		}
		
		/* It has a visible child */
		
		public String childVisible(Class<?> klass,Boolean some,Class<?> child,String parentFieldName) {
			String tupleName = PersistenceHelper.getEntityName(child);
			String variableName = StringHelper.getVariableNameFrom(tupleName);
			return 
					exists(
						select("_as.identifier")
						,from("ActorScope _as")
						,"JOIN Scope child ON _as.scope = child"
						,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = child"
						,where(join(klass, some, "_as",variableName+"."+parentFieldName))
					);
		}
		
		public String childVisible(Class<?> klass,Boolean some,Class<?> child,Class<?> parent) {
			return childVisible(klass, some, child, StringHelper.getVariableNameFrom(parent.getSimpleName()));
		}
		
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
		
		public String parentVisible(Class<?> klass,Boolean some,String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName) {
			String selectFrom = jpql(
					select("p.identifier")
					,from("ActorScope p JOIN Scope s ON p.scope = s")
					,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = s"
			);
			String p1 = Boolean.TRUE.equals(some) ? "p.actor.code = :actorCode" : (Actor.class.equals(klass) ? "p.actor = t" : null);
			String p2 = exists(
					select(variableName)
					,from(tupleName+" "+variableName)
					,where(and(
							variableName+" = t",variableName+"."+fieldName+" = "+fieldName
							,not(
								exists(select("p"+tupleName+" ")+from("ActorScope p"+tupleName+" ")
									+ where(and(join(klass,some, "p"+tupleName, null),visible("p"+tupleName, Boolean.TRUE)))
								)
							)
					))
			);
			return jpql(exists(selectFrom+" WHERE "+(p1 == null ? ConstantEmpty.STRING : p1+" AND ")+p2));
		}
		
		public String parentVisible(Class<?> klass,Boolean some,Class<?> parent,Class<?> child) {
			String parentTupleName = parent.getSimpleName();
			String variableName = StringHelper.getVariableNameFrom(parentTupleName);
			String childTupleName = child.getSimpleName();
			return parentVisible(klass,some,parentTupleName, variableName, childTupleName, StringHelper.getVariableNameFrom(childTupleName), variableName);
		}
		
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
		
		String IS_TYPE_CODE_FORMAT = "t.type.code = '%s'";
		public String isTypeCode(String code) {
			return String.format(IS_TYPE_CODE_FORMAT, code);
		}
		
		String TYPE_CODE_EQUALS = "t.type.code = :typeCode";
		public String typeCodeEquals() {
			return TYPE_CODE_EQUALS;
		}
		
		public String visiblilty(String typeCode,Class<?> klass,Boolean some,Boolean negate) {
			String string = null;
			if(StringHelper.isBlank(typeCode))
				string = parenthesis(or(
						hasVisibleSection(klass,some,negate),hasVisibleAdministrativeUnit(klass,some,negate)
						,hasVisibleBudgetSpecializationUnit(klass,some,negate),hasVisibleAction(klass,some,negate)
						,hasVisibleActivity(klass,some,negate),hasVisibleBudgetCategory(klass,some,negate)
					));
			else if(ScopeType.CODE_SECTION.equals(typeCode))
				string = hasVisibleSection(klass,some,negate);
			else if(ScopeType.CODE_UA.equals(typeCode))
				string = hasVisibleAdministrativeUnit(klass,some,negate);
			else if(ScopeType.CODE_USB.equals(typeCode))
				string = hasVisibleBudgetSpecializationUnit(klass,some,negate);
			else if(ScopeType.CODE_ACTION.equals(typeCode))
				string = hasVisibleAction(klass,some,negate);
			else if(ScopeType.CODE_ACTIVITE.equals(typeCode))
				string = hasVisibleActivity(klass,some,negate);
			else if(ScopeType.CODE_CATEGORIE_BUDGET.equals(typeCode))
				string = hasVisibleBudgetCategory(klass,some,negate);
			
			if(StringHelper.isBlank(string))
				throw new RuntimeException(String.format("Visible predicate of scope type <<%s>> not yet implemented", typeCode));
			return string;
		}
		
		public String scopeVisible(String typeCode,Boolean some,Boolean negate) {
			return visiblilty(typeCode, Scope.class, some, negate);
		}
		
		public String actorView(String typeCode,Boolean some,Boolean negate) {
			return visiblilty(typeCode, Actor.class, some, negate);
		}
		
		public String hasVisibleBudgetCategory(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_CATEGORIE_BUDGET),				
					notIfTrue(parenthesis(or(
						selfVisible(klass,some)
						,childVisible(klass,some,BudgetSpecializationUnit.class,BudgetSpecializationUnit.FIELD_CATEGORY)
					)),negate)
				));
		}
		
		public String hasVisibleActivityCategory(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_CATEGORIE_ACTIVITE),				
					notIfTrue(parenthesis(or(
						selfVisible(klass,some)
						,childVisible(klass,some,Activity.class,ActivityCategory.class)
						,childVisible(klass,some,Imputation.class,ActivityCategory.class)
					)),negate)
				));
		}
		
		public String hasVisibleSection(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_SECTION),				
					notIfTrue(parenthesis(or(
						selfVisible(klass, some)
						/*,childVisible(klass,some,AdministrativeUnit.class,Section.class)
						,childVisible(klass,some,BudgetSpecializationUnit.class,Section.class)
						,childVisible(klass,some,Action.class,Section.class)
						,childVisible(klass,some,Activity.class,Section.class)
						,childVisible(klass,some,Imputation.class,Section.class)*/
					)),negate)
				));
		}
		
		public String hasVisibleAdministrativeUnit(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_UA),
					notIfTrue(parenthesis(or(
						selfVisible(klass, some)
						,parentVisible(klass,some,Section.class, AdministrativeUnit.class)
					)),negate)
				));
		}
		
		public String hasVisibleBudgetSpecializationUnit(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_USB),	
					notIfTrue(parenthesis(or(
						selfVisible(klass, some)
						,parentVisible(klass,some,Section.class, BudgetSpecializationUnit.class)
						,childVisible(klass,some,Action.class,BudgetSpecializationUnit.class)
						,childVisible(klass,some,Activity.class,BudgetSpecializationUnit.class)
						,childVisible(klass,some,Imputation.class,BudgetSpecializationUnit.class)
					)),negate)
				));
		}
		
		public String hasVisibleAction(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTION),	
					notIfTrue(parenthesis(or(
						selfVisible(klass, some)
						,parentVisible(klass,some,Section.class, Action.class)
						,parentVisible(klass,some,BudgetSpecializationUnit.class, Action.class)
						,childVisible(klass,some,Activity.class,Action.class)
						,childVisible(klass,some,Imputation.class,Action.class)
					)),negate)
				));
		}
		
		public String hasVisibleActivity(Class<?> klass,Boolean some,Boolean negate) {
			return parenthesis(and(
					isTypeCode(ScopeType.CODE_ACTIVITE),	
					notIfTrue(parenthesis(or(
						selfVisible(klass, some)
						,parentVisible(klass,some,Section.class, Activity.class)
						,parentVisible(klass,some,AdministrativeUnit.class, Activity.class)
						,parentVisible(klass,some,BudgetSpecializationUnit.class, Activity.class)
						,parentVisible(klass,some,Action.class, Activity.class)
						,childVisible(klass,some,Imputation.class,Activity.class)
					)),negate)
				));
		}
	}
	
	public static interface Predicate {
				
		String PARENT_VARIABLE_NAME = "t";
		String ACTOR_SCOPE_VARIABLE_NAME = "v";
		String PARAMETER_NAME_ACTOR_CODE = "actorCode";
		String PARAMETER_NAME_SCOPE_IDENTIFIER = "scopeIdentifier";
		
		/**
		 * Actor : actors having visibility on any or some scopes<br/>
		 * Scope : visible scopes by any or some actors
		 * @param klass
		 * @param variableName
		 * @param some
		 * @return
		 */
		static String join(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,String variableName,String fieldName) {
			String join = null;
			if(StringHelper.isBlank(fieldName))
				fieldName = variableName+"."+StringHelper.getVariableNameFrom(klass.getSimpleName());
			String natural = fieldName+" = "+ValueHelper.defaultToIfBlank(mainVariableName, klassVariableName);
			if(Actor.class.equals(klass)) {
				if(Boolean.TRUE.equals(some))
					join = and(natural,variableName+".scope.identifier = :"+PARAMETER_NAME_SCOPE_IDENTIFIER);
				else
					join = natural;
			}else if(Scope.class.equals(klass)) {
				if(Boolean.TRUE.equals(some))
					join = and(natural,variableName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE);
				else
					join = natural;
			}
			return join;
		}
		
		static String actor(Boolean parameterable,String variableName,Boolean joinable) {
			return Boolean.TRUE.equals(parameterable) 
					? variableName+".actor.code = :"+PARAMETER_NAME_ACTOR_CODE 
					: (Boolean.TRUE.equals(joinable) ? variableName+".actor = "+PARENT_VARIABLE_NAME : null);
		}
		
		static String scope(Boolean parameterable,String variableName,Boolean joinable) {
			return Boolean.TRUE.equals(parameterable) 
					? variableName+".scope.identifier = :"+PARAMETER_NAME_SCOPE_IDENTIFIER 
					: (Boolean.TRUE.equals(joinable) ? variableName+".scope = "+PARENT_VARIABLE_NAME : null);
		}
		
		String IS_VISIBLE_FORMAT = "(%1$s.visible IS NULL OR %1$s.visible = true)";
		String IS_NOT_VISIBLE_FORMAT = "%1$s.visible IS NOT NULL AND %1$s.visible = false";
		static String visible(String fieldName,Boolean negate) {
			return String.format(Boolean.TRUE.equals(negate) ? IS_NOT_VISIBLE_FORMAT : IS_VISIBLE_FORMAT, fieldName);
		}
		
		static String visible() {
			return visible(ACTOR_SCOPE_VARIABLE_NAME, null);
		}
		
		static String visibleBy(Boolean actorable,Boolean parameterable,String parentFieldName) {
			return and(//join(Tuple.ACTOR, ACTOR_SCOPE_VARIABLE_NAME,parameterable),
				/* Join */FieldHelper.join(ACTOR_SCOPE_VARIABLE_NAME,parentFieldName)+" = t"
				/* Filter by actor code*/,actor(parameterable, ACTOR_SCOPE_VARIABLE_NAME,null) 
				,visible()
			);
		}
		
		static String selfVisible(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some) {
			return exists(select("v.identifier"),from("ActorScope v"),where(and(join(klass,klassVariableName,mainVariableName,some, ACTOR_SCOPE_VARIABLE_NAME,null),visible())));
		}
		
		static String selfVisible(Boolean actorable,Boolean parameterable,String parentFieldName) {
			return exists(select("v.identifier"),from("ActorScope v"),where(visibleBy(actorable,parameterable,parentFieldName)));
		}
		
		/* It has a visible child */
		
		static String childVisible(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Class<?> child,String parentFieldName) {
			String tupleName = PersistenceHelper.getEntityName(child);
			String variableName = StringHelper.getVariableNameFrom(tupleName);
			return 
					exists(
						select("_as.identifier")
						,from("ActorScope _as")
						,"JOIN Scope child ON _as.scope = child"
						,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = child"
						,where(and(
								join(klass,klassVariableName,/*Actor.class.equals(klass) ? "" : */mainVariableName, some, "_as",variableName+"."+parentFieldName)
								,visible("_as", null)
								))
					);
		}
		
		static String childVisible(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Class<?> child,Class<?> parent) {
			return childVisible(klass,klassVariableName,mainVariableName, some, child, StringHelper.getVariableNameFrom(parent.getSimpleName()));
		}
		
		static String childVisible(Boolean actorable,Boolean parameterable,Class<?> klass,String parentFieldName) {
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
		
		static String childVisible(Boolean actorable,Boolean parameterable,Class<?> klass,Class<?> parent) {
			return childVisible(actorable,parameterable, klass, StringHelper.getVariableNameFrom(parent.getSimpleName()));
		}
		
		/* It has a visible parent */
		
		static String parentVisible(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName) {
			String selectFrom = jpql(
					select("p.identifier")
					,from("ActorScope p JOIN Scope s ON p.scope = s")
					,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = s"
			);
			String p1 = Boolean.TRUE.equals(some) ? (Actor.class.equals(klass) ? "p.actor = t" : "p.actor.code = :actorCode") : (Actor.class.equals(klass) ? "p.actor = t" : null);
			String p2 = exists(
					select(variableName)
					,from(tupleName+" "+variableName)
					,where(and(
							variableName+" = "+klassVariableName,variableName+"."+fieldName+" = "+fieldName
							,not(
								exists(select("p"+tupleName+" ")+from("ActorScope p"+tupleName+" ")
									+ where(and(join(klass,klassVariableName,mainVariableName,some, "p"+tupleName, null),visible("p"+tupleName, Boolean.TRUE)))
								)
							)
					))
			);
			return jpql(exists(selectFrom+" WHERE "+(p1 == null ? ConstantEmpty.STRING : p1+" AND ")+p2));
		}
		
		static String parentVisible(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Class<?> parent,Class<?> child) {
			String parentTupleName = parent.getSimpleName();
			String variableName = StringHelper.getVariableNameFrom(parentTupleName);
			String childTupleName = child.getSimpleName();
			return parentVisible(klass,klassVariableName,mainVariableName,some,parentTupleName, variableName, childTupleName, StringHelper.getVariableNameFrom(childTupleName), variableName);
		}
		
		static String parentVisible(Boolean actorable,Boolean parameterable,String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName) {
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
		
		static String parentVisible(Boolean actorable,Boolean parameterable,Class<?> klass,Class<?> child) {
			String parentTupleName = klass.getSimpleName();
			String variableName = StringHelper.getVariableNameFrom(parentTupleName);
			String childTupleName = child.getSimpleName();
			return parentVisible(actorable,parameterable,parentTupleName, variableName, childTupleName, StringHelper.getVariableNameFrom(childTupleName), variableName);
		}
		
		String IS_TYPE_CODE_FORMAT = "%s.type.code = '%s'";
		static String isTypeCode(String klassVariableName,String code) {
			return String.format(IS_TYPE_CODE_FORMAT, klassVariableName,code);
		}
		
		String TYPE_CODE_EQUALS_FORMAT = "%s.type.code = :typeCode";
		static String typeCodeEquals(String klassVariableName) {
			return String.format(TYPE_CODE_EQUALS_FORMAT, klassVariableName);
		}
		
		static String visiblilty(String typeCode,Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			String string = null;
			if(StringHelper.isBlank(typeCode))
				string = parenthesis(or(
						hasVisibleSection(klass,klassVariableName,mainVariableName,some,negate),hasVisibleAdministrativeUnit(klass,klassVariableName,mainVariableName,some,negate)
						,hasVisibleBudgetSpecializationUnit(klass,klassVariableName,mainVariableName,some,negate),hasVisibleAction(klass,klassVariableName,mainVariableName,some,negate)
						,hasVisibleActivity(klass,klassVariableName,mainVariableName,some,negate),hasVisibleBudgetCategory(klass,klassVariableName,mainVariableName,some,negate)
					));
			else if(ScopeType.CODE_SECTION.equals(typeCode))
				string = hasVisibleSection(klass,klassVariableName,mainVariableName,some,negate);
			else if(ScopeType.CODE_UA.equals(typeCode))
				string = hasVisibleAdministrativeUnit(klass,klassVariableName,mainVariableName,some,negate);
			else if(ScopeType.CODE_USB.equals(typeCode))
				string = hasVisibleBudgetSpecializationUnit(klass,klassVariableName,mainVariableName,some,negate);
			else if(ScopeType.CODE_ACTION.equals(typeCode))
				string = hasVisibleAction(klass,klassVariableName,mainVariableName,some,negate);
			else if(ScopeType.CODE_ACTIVITE.equals(typeCode))
				string = hasVisibleActivity(klass,klassVariableName,mainVariableName,some,negate);
			else if(ScopeType.CODE_CATEGORIE_BUDGET.equals(typeCode))
				string = hasVisibleBudgetCategory(klass,klassVariableName,mainVariableName,some,negate);
			if(StringHelper.isBlank(string))
				throw new RuntimeException(String.format("Visible predicate of scope type <<%s>> not yet implemented", typeCode));
			return string;
		}
		
		static String scopeVisible(String typeCode,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return visiblilty(typeCode, Scope.class,klassVariableName,mainVariableName, some, negate);
		}
		
		static String actorView(String typeCode,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return visiblilty(typeCode, Actor.class,klassVariableName,mainVariableName, some, negate);
		}
		
		static String hasVisibleBudgetCategory(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_CATEGORIE_BUDGET),
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName,some)
						,childVisible(klass,klassVariableName,mainVariableName,some,BudgetSpecializationUnit.class,BudgetSpecializationUnit.FIELD_CATEGORY)
						//,childVisible(klass,klassVariableName,mainVariableName,some,Action.class,BudgetCategory.class)
						//,childVisible(klass,klassVariableName,mainVariableName,some,Activity.class,BudgetCategory.class)
					)),negate)
				));
		}
		
		static String hasVisibleActivityCategory(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_CATEGORIE_ACTIVITE),
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName,some)
						,childVisible(klass,klassVariableName,mainVariableName,some,Activity.class,ActivityCategory.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Imputation.class,ActivityCategory.class)
					)),negate)
				));
		}
		
		static String hasVisibleSection(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_SECTION),
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName, some)
						,childVisible(klass,klassVariableName,mainVariableName,some,AdministrativeUnit.class,Section.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,BudgetSpecializationUnit.class,Section.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Action.class,Section.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Activity.class,Section.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Imputation.class,Section.class)
					)),negate)
				));
		}
		
		static String hasVisibleAdministrativeUnit(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_UA),
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName, some)
						,parentVisible(klass,klassVariableName,mainVariableName,some,Section.class, AdministrativeUnit.class)
					)),negate)
				));
		}
		
		static String hasVisibleBudgetSpecializationUnit(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_USB),	
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName, some)
						,parentVisible(klass,klassVariableName,mainVariableName,some,Section.class, BudgetSpecializationUnit.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Action.class,BudgetSpecializationUnit.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Activity.class,BudgetSpecializationUnit.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Imputation.class,BudgetSpecializationUnit.class)
					)),negate)
				));
		}
		
		static String hasVisibleAction(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_ACTION),	
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName, some)
						,parentVisible(klass,klassVariableName,mainVariableName,some,Section.class, Action.class)
						,parentVisible(klass,klassVariableName,mainVariableName,some,BudgetSpecializationUnit.class, Action.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Activity.class,Action.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Imputation.class,Action.class)
					)),negate)
				));
		}
		
		static String hasVisibleActivity(Class<?> klass,String klassVariableName,String mainVariableName,Boolean some,Boolean negate) {
			return parenthesis(and(
					/*Actor.class.equals(klass) ? null : */isTypeCode(klassVariableName,ScopeType.CODE_ACTIVITE),	
					notIfTrue(parenthesis(or(
						selfVisible(klass,klassVariableName,mainVariableName, some)
						,parentVisible(klass,klassVariableName,mainVariableName,some,Section.class, Activity.class)
						,parentVisible(klass,klassVariableName,mainVariableName,some,AdministrativeUnit.class, Activity.class)
						,parentVisible(klass,klassVariableName,mainVariableName,some,BudgetSpecializationUnit.class, Activity.class)
						,parentVisible(klass,klassVariableName,mainVariableName,some,Action.class, Activity.class)
						,childVisible(klass,klassVariableName,mainVariableName,some,Imputation.class,Activity.class)
					)),negate)
				));
		}
	}
}