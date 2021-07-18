package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import javax.persistence.Query;

import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ScopeVisiblesReader extends AbstractScopeVisiblesReader implements Serializable {

	private String typeCode,actorCode;
	private Boolean negate;
	
	@Override
	protected String getVisiblePredicate() {
		return ScopeQueryStringBuilder.Predicate.scopeVisible(typeCode, StringHelper.isBlank(actorCode) ? null : ":"+ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, negate);
	}

	@Override
	protected Boolean isRegisterable() {
		return Boolean.FALSE;
	}
	
	@Override
	protected void setQueryParameters(Query query, Collection<String> identifiers, Map<String, Object> parameters) {
		super.setQueryParameters(query, identifiers, parameters);
		if(StringHelper.isNotBlank(actorCode))
			query.setParameter(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);
	}
}