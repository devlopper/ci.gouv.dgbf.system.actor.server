package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ClusterRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ClusterRepresentationImpl extends AbstractRepresentationEntityImpl<ClusterDto> implements ClusterRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
