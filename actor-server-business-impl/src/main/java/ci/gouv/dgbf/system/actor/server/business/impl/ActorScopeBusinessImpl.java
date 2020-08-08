package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
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
			deleteSections(actor.getCode(),sections.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> administrativeUnits = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_UA)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(administrativeUnits))
			deleteAdministrativeUnits(actor,administrativeUnits.stream().map(x -> x.getCode()).collect(Collectors.toList()));
		
		Collection<Scope> budgetSpecializationUnits = scopes.stream().filter(scope -> scope.getType().getCode().equals(ScopeType.CODE_USB)).collect(Collectors.toList());
		if(CollectionHelper.isNotEmpty(budgetSpecializationUnits))
			deleteBudgetSpecializationUnits(actor,budgetSpecializationUnits.stream().map(x -> x.getCode()).collect(Collectors.toList()));
	}
	
	private void deleteSections(String actorCode,Collection<String> sectionsCodes) {
		if(StringHelper.isBlank(actorCode) || CollectionHelper.isEmpty(sectionsCodes))
			return;
		Collection<ActorScope> actorScopesSections = ActorScopeQuerier.getInstance().readByActorsCodesByScopesCodes(List.of(actorCode),sectionsCodes);
		Collection<ActorScope> actorScopesAdministrativeUnits = ActorScopeQuerier.getInstance().readByActorsCodesByScopeTypesCodes(List.of(actorCode)
				,List.of(ScopeType.CODE_UA));	
		Collection<ActorScope> actorScopesBudgetSpecializationUnits = ActorScopeQuerier.getInstance().readByActorsCodesByScopeTypesCodes(List.of(actorCode)
				,List.of(ScopeType.CODE_USB));	
		if(CollectionHelper.isEmpty(actorScopesSections) && CollectionHelper.isEmpty(actorScopesAdministrativeUnits) && CollectionHelper.isEmpty(actorScopesBudgetSpecializationUnits))
			return;	
		if(CollectionHelper.isNotEmpty(actorScopesSections))
			deleteMany(actorScopesSections);
	
		Collection<String> sectionsIdentifiers = CollectionHelper.isEmpty(sectionsCodes) ? null : ScopeQuerier.getInstance()
				.readByCodesByTypesCodes(sectionsCodes, List.of(ScopeType.CODE_SECTION)).stream().map(x -> x.getIdentifier()).collect(Collectors.toList());
		
		if(CollectionHelper.isNotEmpty(actorScopesAdministrativeUnits)) {			
			actorScopesAdministrativeUnits.forEach(actorScopesAdministrativeUnit -> {
				AdministrativeUnit administrativeUnit = EntityFinder.getInstance().find(AdministrativeUnit.class, actorScopesAdministrativeUnit.getScope().getIdentifier());
				if(sectionsIdentifiers.contains(administrativeUnit.getSection().getIdentifier()))
					delete(actorScopesAdministrativeUnit);
			});
		}
		
		if(CollectionHelper.isNotEmpty(actorScopesBudgetSpecializationUnits)) {
			actorScopesBudgetSpecializationUnits.forEach(actorScopesBudgetSpecializationUnit -> {
				BudgetSpecializationUnit budgetSpecializationUnit = EntityFinder.getInstance().find(BudgetSpecializationUnit.class, actorScopesBudgetSpecializationUnit.getScope().getIdentifier());
				if(sectionsIdentifiers.contains(budgetSpecializationUnit.getSection().getIdentifier()))
					delete(actorScopesBudgetSpecializationUnit);
			});
		}
	}
	
	private void deleteAdministrativeUnits(Actor actor,Collection<String> administrativeUnitsCodes) {
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
		//Collection<ActorScope> actorScopesAdministrativeUnits = ActorScopeQuerier.getInstance().readByActorsCodesByScopesCodes(List.of(actor.getCode()),administrativeUnitsCodes);
		//if(CollectionHelper.isEmpty(actorScopesAdministrativeUnits))
		//	return;
		//deleteMany(actorScopesAdministrativeUnits);
	}
	
	private void deleteBudgetSpecializationUnits(Actor actor,Collection<String> budgetSpecializationUnitsCodes) {
		if(CollectionHelper.isEmpty(budgetSpecializationUnitsCodes))
			return;
		for(String budgetSpecializationUnitsCode : budgetSpecializationUnitsCodes) {
			ActorScope actorScope = ActorScopeQuerier.getInstance().readByActorCodeByScopeCode(actor.getCode(),budgetSpecializationUnitsCode);
			if(actorScope == null) {
				create(new ActorScope().setActor(actor).setScope(__inject__(ScopePersistence.class).readByBusinessIdentifier(budgetSpecializationUnitsCode)).setVisible(Boolean.FALSE));
			}else {
				delete(actorScope);
			}
		}
	}
}