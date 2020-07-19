package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RejectedAccountRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RejectedAccountRequestDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RejectedAccountRequestRepresentationImpl extends AbstractRepresentationEntityImpl<RejectedAccountRequestDto> implements RejectedAccountRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
