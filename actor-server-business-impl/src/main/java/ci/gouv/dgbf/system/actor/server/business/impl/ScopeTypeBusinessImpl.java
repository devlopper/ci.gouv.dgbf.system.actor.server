package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ScopeTypeBusinessImpl extends AbstractBusinessEntityImpl<ScopeType, ScopeTypePersistence> implements ScopeTypeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
