package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.CivilityBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.CivilityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class CivilityBusinessImpl extends AbstractBusinessEntityImpl<Civility, CivilityPersistence> implements CivilityBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
