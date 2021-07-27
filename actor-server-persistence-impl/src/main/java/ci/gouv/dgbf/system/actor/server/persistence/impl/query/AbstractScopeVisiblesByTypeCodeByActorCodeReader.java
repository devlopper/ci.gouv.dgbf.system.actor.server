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
public abstract class AbstractScopeVisiblesByTypeCodeByActorCodeReader extends AbstractScopeVisiblesReader implements Serializable {

	@Override
	protected String getVisiblePredicate() {
		return ScopeQueryStringBuilder.Predicate.scopeVisible(getTypeCode(), true, null);
	}
	
	protected abstract String getTypeCode();
	
	@Override
	protected void setQueryParameters(Query query, Collection<String> identifiers, Map<String, Object> parameters) {
		super.setQueryParameters(query, identifiers, parameters);
		query.setParameter(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, parameters.get(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
	}
}