package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ScopeBusinessImpl extends AbstractBusinessEntityImpl<Scope, ScopePersistence> implements ScopeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
