package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class ScopeActivityVisiblesByActorReader extends AbstractScopeVisiblesByTypeCodeByActorCodeReader implements Serializable {

	@Override
	protected String getTypeCode() {
		return ScopeType.CODE_ACTIVITE;
	}
}