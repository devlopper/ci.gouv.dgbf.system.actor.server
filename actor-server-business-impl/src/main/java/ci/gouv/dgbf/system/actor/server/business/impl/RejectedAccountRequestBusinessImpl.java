package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RejectedAccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RejectedAccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RejectedAccountRequestBusinessImpl extends AbstractBusinessEntityImpl<RejectedAccountRequest, RejectedAccountRequestPersistence> implements RejectedAccountRequestBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
