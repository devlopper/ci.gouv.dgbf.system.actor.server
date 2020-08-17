package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

@ApplicationScoped
public class ActorScopeBusinessImpl extends AbstractBusinessEntityImpl<ActorScope, ActorScopePersistence> implements ActorScopeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenExecuteCreateBefore__(ActorScope actorScope, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(actorScope, properties, function);
		if(StringHelper.isBlank(actorScope.getIdentifier()) && actorScope.getActor() != null && actorScope.getScope() != null && actorScope.getScope().getType() != null)
			actorScope.setIdentifier(actorScope.getActor().getCode()+"_"+actorScope.getScope().getType().getCode()+"_"+actorScope.getScope().getCode());
	}
	
	@Override @Transactional
	public void createByActorByScopes(Actor actor, Collection<Scope> scopes) {
		if(actor == null || CollectionHelper.isEmpty(scopes))
			throw new RuntimeException("acteur et domaine(s) obligatoires");
		Collection<ActorScope> actorScopes = new ArrayList<>();
		scopes.forEach(scope -> {
			ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(), scope.getCode());
			if(actorScope == null)
				actorScope = new ActorScope().setActor(actor).setScope(scope);
			actorScope.setVisible(Boolean.TRUE);
			actorScopes.add(actorScope);
		});
		saveMany(actorScopes);
	}
	
	@Override @Transactional
	public void deleteByActorByScopes(Actor actor,Collection<Scope> scopes) {
		if(actor == null || CollectionHelper.isEmpty(scopes))
			return;
		Collection<Scope> sections = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_SECTION)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(sections))
			deleteSections(actor,sections.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> budgetSpecializationUnits = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_USB)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(budgetSpecializationUnits))
			deleteBudgetSpecializationUnits(actor,budgetSpecializationUnits.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> activities = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_ACTIVITE)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(activities))
			deleteActivities(actor,activities.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> imputations = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_IMPUTATION)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(imputations))
			deleteImputations(actor,imputations.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> administrativeUnits = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_UA)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(administrativeUnits))
			deleteAdministrativeUnits(actor,administrativeUnits.stream().map(x -> x.getCode()).collect(Collectors.toList()));
	}
	
	private void deleteSections(Actor actor,Collection<String> sectionsCodes) {
		Collection<Object[]> childrenInfos = new ArrayList<>();
		childrenInfos.add(new Object[] {BudgetSpecializationUnit.class,ScopeType.CODE_USB,null});
		childrenInfos.add(new Object[] {Activity.class,ScopeType.CODE_ACTIVITE,null});
		childrenInfos.add(new Object[] {Imputation.class,ScopeType.CODE_IMPUTATION,null});
		delete(actor, ScopeType.CODE_USB, sectionsCodes, childrenInfos,null);
	}
	
	private void deleteAdministrativeUnits(Actor actor,Collection<String> administrativeUnitsCodes) {
		delete(actor, ScopeType.CODE_UA, administrativeUnitsCodes, null,new DeleteListener.AbstractImpl<AdministrativeUnit>() {			
			@Override
			protected Class<AdministrativeUnit> getKlass() {
				return AdministrativeUnit.class;
			}
			
			@Override
			protected Collection<String> getParentsFieldsNames(AdministrativeUnit administrativeUnit) {
				return List.of(AdministrativeUnit.FIELD_SECTION);
			}
			/*@Override
			public Boolean isDeletable(ActorScope actorScope) {
				AdministrativeUnit administrativeUnit = EntityFinder.getInstance().find(AdministrativeUnit.class, actorScope.getScope().getIdentifier());
				if(administrativeUnit == null)
					return Boolean.TRUE;				
				Scope scopeSection = EntityFinder.getInstance().find(Scope.class, administrativeUnit.getSection().getIdentifier());
				if(ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),scopeSection.getCode()) == null)
					return Boolean.TRUE;		
				return Boolean.FALSE;
			}*/
		});
	}
	
	private void deleteBudgetSpecializationUnits(Actor actor,Collection<String> budgetSpecializationUnitsCodes) {
		Collection<Object[]> childrenInfos = new ArrayList<>();
		childrenInfos.add(new Object[] {Activity.class,ScopeType.CODE_ACTIVITE,null});
		childrenInfos.add(new Object[] {Imputation.class,ScopeType.CODE_IMPUTATION,null});
		delete(actor, ScopeType.CODE_USB, budgetSpecializationUnitsCodes, childrenInfos,new DeleteListener.AbstractImpl<BudgetSpecializationUnit>() {
			@Override
			protected Class<BudgetSpecializationUnit> getKlass() {
				return BudgetSpecializationUnit.class;
			}
			
			@Override
			protected Collection<String> getParentsFieldsNames(BudgetSpecializationUnit budgetSpecializationUnit) {
				return List.of(BudgetSpecializationUnit.FIELD_SECTION);
			}
			/*
			@Override
			public Boolean isDeletable(ActorScope actorScope) {
				BudgetSpecializationUnit budgetSpecializationUnit = get(actorScope);
				if(budgetSpecializationUnit == null)
					return Boolean.TRUE;
				Scope scopeSection = EntityFinder.getInstance().find(Scope.class, budgetSpecializationUnit.getSection().getIdentifier());
				if(ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),scopeSection.getCode()) == null)
					return Boolean.TRUE;		
				return Boolean.FALSE;
			}
			*/
			
		});
	}
	
	private void deleteActivities(Actor actor,Collection<String> activitiesCodes) {
		Collection<Object[]> childrenInfos = new ArrayList<>();
		childrenInfos.add(new Object[] {Imputation.class,ScopeType.CODE_IMPUTATION,null});
		delete(actor, ScopeType.CODE_ACTIVITE, activitiesCodes, childrenInfos,new DeleteListener.AbstractImpl<Activity>() {
			@Override
			protected Class<Activity> getKlass() {
				return Activity.class;
			}
			
			@Override
			protected Collection<String> getParentsFieldsNames(Activity activity) {
				return List.of(Activity.FIELD_BUDGET_SPECIALIZATION_UNIT,Activity.FIELD_SECTION);
			}		
		});
	}
	
	private void deleteImputations(Actor actor,Collection<String> imputationsCodes) {
		delete(actor, ScopeType.CODE_IMPUTATION, imputationsCodes, null,null);
	}
	
	/**/
	
	private void delete(Actor actor,String typeCode,Collection<String> codes,Collection<Object[]> childrenInfos,DeleteListener<?> listener) {
		if(actor == null || CollectionHelper.isEmpty(codes))
			return;
		//Collection<ActorScope> actorScopes = ActorScopeQuerier.getInstance().readByActorsCodesByScopesCodes(List.of(actor.getCode()),codes);
		for(String code : codes) {
			ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),code);
			if(actorScope == null) {
				create(new ActorScope().setActor(actor).setScope(__inject__(ScopePersistence.class).readByBusinessIdentifier(code)).setVisible(Boolean.FALSE));
			}else {
				if(listener == null || Boolean.TRUE.equals(listener.isDeletable(actorScope)))
					delete(actorScope);
				else
					update(actorScope.setVisible(Boolean.FALSE));
			}
		}
		
		if(CollectionHelper.isNotEmpty(childrenInfos))
			for(Object[] childInfo : childrenInfos)
				childInfo[2] = ActorScopeQuerier.getInstance().readByActorsCodesByScopeTypesCodes(List.of(actor.getCode()),List.of((String)childInfo[1]));
		
		//if(CollectionHelper.isNotEmpty(actorScopes))
		//	deleteMany(actorScopes);
		
		if(CollectionHelper.isNotEmpty(childrenInfos)) {
			Collection<String> identifiers = CollectionHelper.cast(String.class, FieldHelper.readSystemIdentifiers(ScopeQuerier.getInstance().readByCodesByTypesCodes(codes, List.of(typeCode))));
			for(Object[] childInfo : childrenInfos) {
				@SuppressWarnings("unchecked")
				Collection<ActorScope> children = (Collection<ActorScope>) childInfo[2];
				if(CollectionHelper.isEmpty(children))
					continue;
				for(ActorScope child : children) {
					if(childInfo[0].equals(Imputation.class)) {
						Imputation imputation = EntityFinder.getInstance().find(Imputation.class, child.getScope().getIdentifier());
						if(ScopeType.CODE_ACTIVITE.equals(typeCode) && !identifiers.contains(imputation.getActivity().getIdentifier()))
							continue;
					}else if(childInfo[0].equals(Activity.class)) {
						Activity activity = EntityFinder.getInstance().find(Activity.class, child.getScope().getIdentifier());
						if(ScopeType.CODE_USB.equals(typeCode) && !identifiers.contains(activity.getBudgetSpecializationUnit().getIdentifier()))
							continue;
					}
					delete(child);
				}				
			}		
		}
	}
	
	public static interface DeleteListener<T> {
		Boolean isDeletable(ActorScope actorScope);
		
		public static abstract class AbstractImpl<SCOPE> implements DeleteListener<SCOPE>,Serializable{
			protected abstract Class<SCOPE> getKlass();
			
			protected SCOPE get(ActorScope actorScope) {
				return EntityFinder.getInstance().find(getKlass(), actorScope.getScope().getIdentifier());
			}
			
			protected abstract Collection<String> getParentsFieldsNames(SCOPE scope);
			
			protected Object getParentIdentifier(SCOPE scope,String fieldName) {
				Collection<String> parentsFieldsNames = getParentsFieldsNames(scope);
				if(CollectionHelper.isEmpty(parentsFieldsNames))
					return null;
				for(String parentFieldName : parentsFieldsNames) {
					if(StringHelper.isBlank(parentFieldName))
						continue;
					Object parent = FieldHelper.read(scope, parentFieldName);
					if(parent == null)
						continue;
					return FieldHelper.readSystemIdentifier(parent);
				}
				return null;
			}
			
			protected Scope getParent(SCOPE scope,String fieldName) {
				Object parentIdentifier = getParentIdentifier(scope,fieldName);
				if(parentIdentifier == null)
					return null;
				return EntityFinder.getInstance().find(Scope.class, parentIdentifier);
			}
			
			@Override
			public Boolean isDeletable(ActorScope actorScope) {
				SCOPE scope = get(actorScope);
				if(scope == null)
					return Boolean.TRUE;
				for(String fieldName : getParentsFieldsNames(scope)) {
					Scope parent = getParent(scope,fieldName);
					if(parent == null)
						continue;
					if(ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actorScope.getActor().getCode(),parent.getCode()) != null)
						return Boolean.FALSE;
				}
				return Boolean.FALSE;
			}
		}
	}
}