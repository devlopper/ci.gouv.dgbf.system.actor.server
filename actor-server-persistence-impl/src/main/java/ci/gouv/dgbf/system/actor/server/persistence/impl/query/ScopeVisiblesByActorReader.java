package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import javax.persistence.Query;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ScopeVisiblesByActorReader extends AbstractScopeVisiblesReader implements Serializable {

	@Override
	protected String getVisiblePredicate() {
		return null;//VisibilityQueryStringBuilder.Predicate.scopeVisible(null, Boolean.TRUE,Boolean.TRUE, null);
	}
	
	@Override
	protected void setQueryParameters(Query query, Collection<String> identifiers, Map<String, Object> parameters) {
		super.setQueryParameters(query, identifiers, parameters);
		query.setParameter(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, parameters.get(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
	}
}