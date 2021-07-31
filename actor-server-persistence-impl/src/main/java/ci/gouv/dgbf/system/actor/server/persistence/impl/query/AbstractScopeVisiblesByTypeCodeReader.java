package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public abstract class AbstractScopeVisiblesByTypeCodeReader extends AbstractScopeVisiblesReader implements Serializable {

	@Override
	protected String getVisiblePredicate() {
		return VisibilityQueryStringBuilder.Predicate.scopeVisible(getTypeCode(), null, null,null);
	}
	
	protected abstract String getTypeCode();
}