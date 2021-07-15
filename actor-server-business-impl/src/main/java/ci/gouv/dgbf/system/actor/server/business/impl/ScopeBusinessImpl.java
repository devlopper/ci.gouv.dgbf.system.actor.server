package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

@ApplicationScoped
public class ScopeBusinessImpl extends AbstractBusinessEntityImpl<Scope, ScopePersistence> implements ScopeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Collection<Scope> getVisibleSections(String actorCode) {
		return getVisibles(actorCode, ScopeOfTypeSectionQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}

	@Override
	public Collection<Scope> getVisibleAdministrativeUnits(String actorCode) {
		return getVisibles(actorCode, ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}

	@Override
	public Collection<Scope> getVisibleBudgetSpecializationUnits(String actorCode) {
		return getVisibles(actorCode, ScopeOfTypeBudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	}
	
	/**/
	
	private static Collection<Scope> getVisibles(String actorCode,String queryIdentifier) {
		Collection<Scope> scopes = EntityReader.getInstance().readMany(Scope.class,new QueryExecutorArguments().setQuery(new Query().setIdentifier(queryIdentifier))
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode));
		if(CollectionHelper.isEmpty(scopes))
			return null;
		return scopes;
	}
}