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
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityEconomicNature;
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
		
		Collection<Scope> administrativeUnits = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_UA)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(administrativeUnits))
			deleteAdministrativeUnits(actor,administrativeUnits.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> budgetSpecializationUnits = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_USB)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(budgetSpecializationUnits))
			deleteBudgetSpecializationUnits(actor,budgetSpecializationUnits.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> activities = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_ACTIVITE)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(activities))
			deleteActivities(actor,activities.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> imputations = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_IMPUTATION)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(imputations))
			deleteImputations(actor,imputations.stream().map(x -> x.getCode()).collect(Collectors.toList()));
	}
	
	private void deleteSections(Actor actor,Collection<String> sectionsCodes) {
		Collection<Object[]> childrenInfos = new ArrayList<>();
		childrenInfos.add(new Object[] {BudgetSpecializationUnit.class,ScopeType.CODE_USB,null});
		childrenInfos.add(new Object[] {Activity.class,ScopeType.CODE_ACTIVITE,null});
		childrenInfos.add(new Object[] {ActivityEconomicNature.class,ScopeType.CODE_IMPUTATION,null});
		delete(actor, ScopeType.CODE_USB, sectionsCodes, childrenInfos,null);
	}
	
	private void deleteAdministrativeUnits(Actor actor,Collection<String> administrativeUnitsCodes) {
		delete(actor, ScopeType.CODE_UA, administrativeUnitsCodes, null,new DeleteListener() {			
			@Override
			public Boolean isDeletable(ActorScope actorScope) {
				AdministrativeUnit administrativeUnit = EntityFinder.getInstance().find(AdministrativeUnit.class, actorScope.getScope().getIdentifier());
				if(administrativeUnit == null)
					return Boolean.TRUE;				
				Scope scopeSection = EntityFinder.getInstance().find(Scope.class, administrativeUnit.getSection().getIdentifier());
				if(ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),scopeSection.getCode()) == null)
					return Boolean.TRUE;		
				return Boolean.FALSE;
			}
		});
		/*
		if(CollectionHelper.isEmpty(administrativeUnitsCodes))
			return;
		for(String administrativeUnitsCode : administrativeUnitsCodes) {
			ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),administrativeUnitsCode);
			if(actorScope == null) {
				create(new ActorScope().setActor(actor).setScope(__inject__(ScopePersistence.class).readByBusinessIdentifier(administrativeUnitsCode)).setVisible(Boolean.FALSE));
			}else {
				ActorScope actorScopeSection = null;
				AdministrativeUnit administrativeUnit = EntityFinder.getInstance().find(AdministrativeUnit.class, actorScope.getScope().getIdentifier());
				if(administrativeUnit != null) {
					Scope scopeSection = EntityFinder.getInstance().find(Scope.class, administrativeUnit.getSection().getIdentifier());
					if(scopeSection != null)
						actorScopeSection = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),scopeSection.getCode());	
				}			
				if(actorScopeSection == null)
					delete(actorScope);
				else
					update(actorScope.setVisible(Boolean.FALSE));
			}
		}
		*/
	}
	
	private void deleteBudgetSpecializationUnits(Actor actor,Collection<String> budgetSpecializationUnitsCodes) {
		Collection<Object[]> childrenInfos = new ArrayList<>();
		childrenInfos.add(new Object[] {Activity.class,ScopeType.CODE_ACTIVITE,null});
		childrenInfos.add(new Object[] {ActivityEconomicNature.class,ScopeType.CODE_IMPUTATION,null});
		delete(actor, ScopeType.CODE_USB, budgetSpecializationUnitsCodes, childrenInfos,null);
	}
	
	private void deleteActivities(Actor actor,Collection<String> activitiesCodes) {
		Collection<Object[]> childrenInfos = new ArrayList<>();
		childrenInfos.add(new Object[] {ActivityEconomicNature.class,ScopeType.CODE_IMPUTATION,null});
		delete(actor, ScopeType.CODE_ACTIVITE, activitiesCodes, childrenInfos,null);
	}
	
	private void deleteImputations(Actor actor,Collection<String> imputationsCodes) {
		delete(actor, ScopeType.CODE_IMPUTATION, imputationsCodes, null,null);
	}
	
	/**/
	
	private void delete(Actor actor,String typeCode,Collection<String> codes,Collection<Object[]> childrenInfos,DeleteListener listener) {
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
					if(childInfo[0].equals(ActivityEconomicNature.class)) {
						ActivityEconomicNature activityEconomicNature = EntityFinder.getInstance().find(ActivityEconomicNature.class, child.getScope().getIdentifier());
						if(ScopeType.CODE_ACTIVITE.equals(typeCode) && !identifiers.contains(activityEconomicNature.getActivity().getIdentifier()))
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
	
	public static interface DeleteListener {
		Boolean isDeletable(ActorScope actorScope);
	}
}